// Modern Objective-C: Network manager with blocks and GCD
#import <Foundation/Foundation.h>

NS_ASSUME_NONNULL_BEGIN

typedef NS_ENUM(NSInteger, MCNetworkStatus) {
    MCNetworkStatusUnknown,
    MCNetworkStatusOffline,
    MCNetworkStatusOnline,
    MCNetworkStatusDegraded
};

typedef void (^MCCompletionHandler)(NSData * _Nullable data,
                                     NSURLResponse * _Nullable response,
                                     NSError * _Nullable error);

@protocol MCCacheDelegate <NSObject>
- (nullable NSData *)cachedDataForKey:(NSString *)key;
- (void)cacheData:(NSData *)data forKey:(NSString *)key ttl:(NSTimeInterval)ttl;
@end

@interface MCNetworkManager : NSObject

@property (nonatomic, readonly) MCNetworkStatus status;
@property (nonatomic, weak, nullable) id<MCCacheDelegate> cacheDelegate;
@property (nonatomic, assign) NSTimeInterval timeout;

+ (instancetype)sharedManager;
- (instancetype)init NS_UNAVAILABLE;

- (void)GET:(NSString *)endpoint
 parameters:(nullable NSDictionary<NSString *, id> *)params
 completion:(MCCompletionHandler)completion;

- (void)POST:(NSString *)endpoint
        body:(nullable NSDictionary *)body
  completion:(MCCompletionHandler)completion;

@end

@implementation MCNetworkManager {
    NSURLSession *_session;
    dispatch_queue_t _processingQueue;
    NSMutableDictionary<NSString *, NSURLSessionTask *> *_activeTasks;
}

+ (instancetype)sharedManager {
    static MCNetworkManager *manager = nil;
    static dispatch_once_t onceToken;
    dispatch_once(&onceToken, ^{
        manager = [[self alloc] initPrivate];
    });
    return manager;
}

- (instancetype)initPrivate {
    self = [super init];
    if (self) {
        NSURLSessionConfiguration *config = [NSURLSessionConfiguration defaultSessionConfiguration];
        config.timeoutIntervalForRequest = 30.0;
        config.HTTPAdditionalHeaders = @{
            @"User-Agent": @"MarkCraft/1.5.0",
            @"Accept": @"application/json"
        };
        _session = [NSURLSession sessionWithConfiguration:config];
        _processingQueue = dispatch_queue_create("fr.markcraft.network", DISPATCH_QUEUE_CONCURRENT);
        _activeTasks = [NSMutableDictionary new];
        _timeout = 30.0;
    }
    return self;
}

- (void)GET:(NSString *)endpoint
 parameters:(NSDictionary<NSString *, id> *)params
 completion:(MCCompletionHandler)completion {
    
    NSURLComponents *components = [NSURLComponents componentsWithString:endpoint];
    if (params) {
        NSMutableArray<NSURLQueryItem *> *items = [NSMutableArray new];
        [params enumerateKeysAndObjectsUsingBlock:^(NSString *key, id value, BOOL *stop) {
            [items addObject:[NSURLQueryItem queryItemWithName:key
                                                        value:[value description]]];
        }];
        components.queryItems = items;
    }
    
    NSURLRequest *request = [NSURLRequest requestWithURL:components.URL];
    NSURLSessionDataTask *task = [_session dataTaskWithRequest:request
                                            completionHandler:^(NSData *data, NSURLResponse *resp, NSError *err) {
        dispatch_async(self->_processingQueue, ^{
            completion(data, resp, err);
        });
    }];
    [task resume];
}

@end
NS_ASSUME_NONNULL_END
