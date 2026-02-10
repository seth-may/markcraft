import SwiftUI
import AppKit
import Foundation
import Combine
import AVFoundation
import UniformTypeIdentifiers
import OSLog

// MARK: - 📄 SyntaxEngine.swift
// ════════════════════════════════════════════════════════════

// SyntaxEngine.swift — MarkCraft IDE V101 — 60-language Syntax Engine
// Entièrement généré par Claude (Anthropic)

// ══════════════════════════════════════════════════
// MARK: - Syntax Language Definition
// ══════════════════════════════════════════════════
struct SynLang: Identifiable {
    let id = UUID()
    let name, ext, icon: String; let color: Color
    let kw: Set<String>; let types: Set<String>; let slc: String
    var category: LangCategory = .general
}

enum LangCategory: String, CaseIterable {
    case systems = "Systems"
    case web = "Web"
    case mobile = "Mobile"
    case scripting = "Scripting"
    case functional = "Functional"
    case data = "Data"
    case markup = "Markup"
    case devops = "DevOps"
    case general = "General"
}

// ══════════════════════════════════════════════════
// MARK: - 60 Languages Registry
// ══════════════════════════════════════════════════
enum Syn {
    // Helper to build language quickly
    private static func L(_ name: String, _ ext: String, _ icon: String, _ color: Color, _ cat: LangCategory,
                          _ kw: [String], _ types: [String], _ slc: String) -> SynLang {
        SynLang(name:name, ext:ext, icon:icon, color:color, kw:Set(kw), types:Set(types), slc:slc, category:cat)
    }

    static let all: [SynLang] = [
        // ── 1. Swift ──
        L("Swift","swift","swift",.orange,.mobile,
          ["func","var","let","if","else","for","while","return","import","struct","class","enum","protocol","guard","switch","case","break","continue","defer","do","try","catch","throw","async","await","// @main (example)","@State","@Published","@Observable","@Environment","@AppStorage","@Binding","@StateObject","init","self","super","true","false","nil","in","some","override","private","public","internal","static","final","lazy","mutating","fileprivate","open","indirect","convenience","required","optional","dynamic","willSet","didSet","get","set","associatedtype","typealias","where","inout","rethrows","subscript","actor","distributed","macro","any","each","repeat","consume","discard","borrowing","consuming","nonisolated","isolated"],
          ["String","Int","Bool","Double","Float","Array","View","Body","Scene","Color","Text","VStack","HStack","ZStack","Image","Button","NavigationStack","List","ForEach","ScrollView","LazyVStack","GeometryReader","Binding","Published","StateObject","ObservableObject","Identifiable","Codable","Hashable","Equatable","Error","Result","Optional","Dictionary","Set","Data","URL","Date","UUID","CGFloat","CGPoint","CGSize","CGRect","AnyView","Task","AsyncStream","MainActor","Sendable"],"//"),
        // ── 2. Python ──
        L("Python","py","chevron.left.forwardslash.chevron.right",.blue,.scripting,
          ["def","class","if","elif","else","for","while","return","import","from","as","with","try","except","finally","raise","pass","break","continue","yield","lambda","and","or","not","in","is","True","False","None","self","cls","print","async","await","global","nonlocal","del","assert","super","property","staticmethod","classmethod","abstractmethod","dataclass","match","case","type"],
          ["int","str","float","bool","list","dict","tuple","set","bytes","type","object","range","complex","frozenset","Optional","Union","Any","Sequence","Mapping","Iterator","Generator","Coroutine","TypeVar","Generic","Protocol","Final","Literal","TypedDict","NamedTuple"],"#"),
        // ── 3. JavaScript ──
        L("JavaScript","js","globe",.yellow,.web,
          ["function","const","let","var","if","else","for","while","return","import","export","from","class","new","this","try","catch","throw","finally","async","await","switch","case","break","continue","default","true","false","null","undefined","typeof","instanceof","console","document","window","require","module","of","in","do","delete","void","yield","super","extends","static","get","set"],
          ["Array","Object","String","Number","Boolean","Promise","Map","Set","WeakMap","WeakSet","Date","Error","RegExp","Symbol","BigInt","Proxy","JSON","Math","URL","FormData","Headers","Request","Response","Blob","File","ReadableStream","WritableStream"],"//"),
        // ── 4. TypeScript ──
        L("TypeScript","ts","t.square",.blue,.web,
          ["function","const","let","var","if","else","for","while","return","import","export","class","new","this","interface","type","enum","extends","implements","async","await","true","false","null","undefined","keyof","typeof","readonly","public","private","protected","abstract","declare","namespace","module","satisfies","as","in","of","is","infer","using","override"],
          ["string","number","boolean","any","void","never","unknown","object","symbol","bigint","Array","Promise","Record","Partial","Omit","Pick","Required","Readonly","Exclude","Extract","NonNullable","Parameters","ReturnType","Map","Set","WeakMap"],"//"),
        // ── 5. JSX ──
        L("JSX","jsx","globe",.cyan,.web,
          ["function","const","let","var","if","else","for","while","return","import","export","from","class","new","this","try","catch","throw","async","await","switch","case","true","false","null","undefined"],
          ["React","useState","useEffect","useRef","useMemo","useCallback","useContext","useReducer","Component","Fragment","FC","Suspense","StrictMode","lazy","memo","forwardRef","createContext"],"//"),
        // ── 6. TSX ──
        L("TSX","tsx","t.square",.cyan,.web,
          ["function","const","let","var","if","else","for","while","return","import","export","class","new","this","interface","type","async","await","true","false","null","undefined","keyof","typeof","readonly","as","satisfies"],
          ["React","useState","useEffect","useRef","useMemo","FC","Props","string","number","boolean","any","JSX","ReactNode","SetStateAction","Dispatch","ChangeEvent"],"//"),
        // ── 7. HTML ──
        L("HTML","html","chevron.left.forwardslash.chevron.right",.orange,.markup,
          ["html","head","body","div","span","p","a","img","ul","ol","li","table","tr","td","th","form","input","button","select","option","textarea","script","style","link","meta","title","h1","h2","h3","h4","h5","h6","nav","header","footer","main","section","article","aside","figure","details","summary","dialog","canvas","video","audio","source","iframe","template","slot","svg","path"],
          ["class","id","href","src","alt","type","name","value","style","data","aria","role","tabindex","placeholder","required","disabled","checked","hidden","autofocus","action","method","target","rel","width","height","loading"],""),
        // ── 8. CSS ──
        L("CSS","css","paintbrush",.pink,.web,
          ["color","background","margin","padding","border","border-radius","display","flex","flex-direction","grid","grid-template-columns","position","width","height","min-width","max-width","font-size","font-weight","font-family","text-align","text-decoration","line-height","align-items","justify-content","gap","overflow","opacity","transition","transform","animation","hover","focus","active","media","import","keyframes","var","calc","z-index","cursor","box-sizing","box-shadow","outline","aspect-ratio","object-fit","filter","backdrop-filter","clip-path","container","has","is","where"],
          ["px","rem","em","vh","vw","fr","%","deg","s","ms","rgb","rgba","hsl","hsla","var","calc","clamp","min","max","none","auto","inherit","initial","unset","flex","grid","block","inline","hidden","visible","fixed","absolute","relative","sticky","center","cover","contain"],""),
        // ── 9. Java ──
        L("Java","java","cup.and.saucer",.red,.systems,
          ["public","private","protected","static","final","void","class","interface","extends","implements","new","return","if","else","for","while","do","try","catch","throw","throws","finally","import","package","this","super","true","false","null","abstract","synchronized","volatile","transient","break","continue","default","switch","case","instanceof","record","sealed","permits","yield","var","enum"],
          ["String","int","boolean","double","float","long","short","byte","char","Integer","Boolean","Double","Float","Long","Object","List","ArrayList","Map","HashMap","Set","HashSet","Queue","Optional","Stream","Collectors","Function","Predicate","Consumer","Supplier","CompletableFuture","Thread","Runnable","Callable","Path","File","Files","BigDecimal","LocalDate","LocalDateTime"],"//"),
        // ── 10. C ──
        L("C","c","c.square",.gray,.systems,
          ["int","char","float","double","void","short","long","unsigned","signed","if","else","for","while","do","switch","case","break","continue","return","struct","union","typedef","enum","const","static","extern","volatile","register","auto","sizeof","inline","restrict","_Atomic","_Generic","_Static_assert","NULL","true","false","printf","scanf","malloc","calloc","realloc","free","memcpy","memmove","memset","strlen","strcpy","strcmp","fopen","fclose","fread","fwrite","include","define","ifdef","ifndef","endif","pragma"],
          ["FILE","size_t","ssize_t","ptrdiff_t","int8_t","int16_t","int32_t","int64_t","uint8_t","uint16_t","uint32_t","uint64_t","bool","pid_t","off_t","time_t","va_list"],"//"),
        // ── 11. C++ ──
        L("C++","cpp","plus.square",.blue,.systems,
          ["int","char","void","bool","if","else","for","while","do","return","class","struct","union","namespace","using","template","typename","virtual","override","final","public","private","protected","new","delete","try","catch","throw","auto","const","constexpr","consteval","static","extern","inline","volatile","mutable","explicit","friend","operator","this","nullptr","true","false","sizeof","decltype","noexcept","co_await","co_return","co_yield","concept","requires","module","import","switch","case","break","continue","include","define"],
          ["size_t","string","string_view","vector","array","deque","list","map","unordered_map","set","unordered_set","pair","tuple","optional","variant","any","span","shared_ptr","unique_ptr","weak_ptr","function","thread","mutex","lock_guard","unique_lock","condition_variable","future","promise","async","atomic","chrono","filesystem","path","cout","cin","cerr","endl","regex"],"//"),
        // ── 12. C# ──
        L("C#","cs","c.square",.green,.systems,
          ["using","namespace","class","struct","interface","enum","record","if","else","for","foreach","while","do","return","new","this","base","public","private","protected","internal","static","void","var","const","readonly","async","await","try","catch","throw","finally","true","false","null","override","virtual","abstract","sealed","partial","yield","switch","case","break","continue","default","delegate","event","is","as","in","out","ref","params","lock","where","when","and","or","not","with","required","init","get","set"],
          ["string","int","bool","double","float","long","decimal","object","dynamic","List","Dictionary","HashSet","Queue","Stack","Task","ValueTask","Func","Action","IEnumerable","IQueryable","IAsyncEnumerable","IDisposable","Span","ReadOnlySpan","Memory","StringBuilder","Regex","DateTime","DateTimeOffset","Guid","Uri","Type","Exception","HttpClient","CancellationToken","Channel"],"//"),
        // ── 13. Rust ──
        L("Rust","rs","r.square",.orange,.systems,
          ["fn","let","mut","if","else","for","while","loop","match","return","use","mod","pub","crate","struct","enum","impl","trait","self","Self","super","as","in","ref","move","async","await","unsafe","true","false","type","const","static","extern","where","dyn","Some","None","Ok","Err","break","continue","yield","macro_rules","println","eprintln","format","vec","panic","todo","unimplemented","assert","assert_eq","derive","allow","test","cfg"],
          ["i8","i16","i32","i64","i128","u8","u16","u32","u64","u128","f32","f64","bool","char","str","String","Vec","Box","Rc","Arc","Cell","RefCell","Mutex","RwLock","Option","Result","HashMap","HashSet","BTreeMap","BTreeSet","VecDeque","Cow","Pin","PhantomData","usize","isize","PathBuf","Path","File","BufReader","BufWriter","Read","Write","Display","Debug","Clone","Copy","Send","Sync","Sized","Drop","Default","From","Into","Iterator","Future","Error","Duration","Instant"],"//"),
        // ── 14. Go ──
        L("Go","go","g.square",.cyan,.systems,
          ["func","var","const","if","else","for","range","switch","case","return","import","package","type","struct","interface","map","chan","go","defer","select","default","true","false","nil","break","continue","fallthrough","goto","fmt","Println","Printf","Sprintf","make","append","len","cap","new","copy","delete","close","panic","recover","any","comparable"],
          ["string","int","int8","int16","int32","int64","uint","uint8","uint16","uint32","uint64","float32","float64","bool","byte","rune","error","any","comparable","context","Context","Reader","Writer","Closer","Handler","Server","Client","Request","Response","File","WaitGroup","Mutex","RWMutex","Once","Duration","Time"],"//"),
        // ── 15. Ruby ──
        L("Ruby","rb","diamond",.red,.scripting,
          ["def","class","module","if","elsif","else","unless","while","until","for","do","end","return","require","require_relative","include","extend","self","super","yield","true","false","nil","and","or","not","begin","rescue","ensure","raise","retry","puts","print","attr_accessor","attr_reader","attr_writer","lambda","proc","freeze","then","when","case","in","next","break","protected","private","public"],
          ["String","Integer","Float","Array","Hash","Symbol","Proc","Regexp","Range","IO","File","Dir","Struct","OpenStruct","Set","Enumerable","Comparable","Object","Module","Class","NilClass","Encoding","Exception","StandardError","Fiber","Thread","Mutex","Enumerator"],"#"),
        // ── 16. PHP ──
        L("PHP","php","p.square",.purple,.web,
          ["function","class","interface","trait","enum","if","elseif","else","for","foreach","while","do","switch","case","default","break","continue","return","echo","print","require","require_once","include","include_once","use","namespace","new","this","self","parent","static","public","private","protected","abstract","final","const","true","false","null","array","match","fn","throw","try","catch","finally","yield","readonly","mixed","void"],
          ["string","int","float","bool","array","object","callable","iterable","void","mixed","never","Closure","Generator","Fiber","WeakReference","WeakMap","DateTime","DateTimeImmutable","Exception","Error","TypeError","ValueError","PDO","PDOStatement","SimpleXMLElement"],"//"),
        // ── 17. Kotlin ──
        L("Kotlin","kt","k.square",.purple,.mobile,
          ["fun","val","var","if","else","when","for","while","do","return","class","object","interface","data","sealed","enum","annotation","inner","import","this","super","is","as","in","try","catch","finally","throw","true","false","null","override","open","abstract","final","lateinit","by","lazy","companion","init","constructor","get","set","suspend","inline","crossinline","reified","value","typealias","where","break","continue","println","print"],
          ["String","Int","Long","Short","Byte","Double","Float","Boolean","Char","Unit","Any","Nothing","Array","IntArray","LongArray","List","MutableList","Map","MutableMap","HashMap","Set","MutableSet","HashSet","Pair","Triple","Sequence","Collection","Comparable","Throwable","Exception","Regex","Result","Channel","Flow","StateFlow","SharedFlow","CoroutineScope","Job","Deferred","Duration"],"//"),
        // ── 18. Dart ──
        L("Dart","dart","d.square",.cyan,.mobile,
          ["void","var","final","const","late","if","else","for","while","do","switch","case","default","return","class","extends","implements","with","abstract","interface","mixin","import","export","async","await","try","catch","finally","throw","rethrow","new","this","super","true","false","null","required","factory","get","set","static","typedef","as","is","in","assert","break","continue","sealed","enum","extension","when"],
          ["String","int","double","bool","num","List","Map","Set","Iterable","Symbol","Type","Function","Object","dynamic","Never","Record","Future","FutureOr","Stream","Completer","StreamController","Timer","Duration","DateTime","Uri","RegExp","BigInt","Enum"],"//"),
        // ── 19. Shell ──
        L("Shell","sh","terminal",.green,.devops,
          ["echo","if","then","else","elif","fi","for","while","until","do","done","case","esac","in","function","return","exit","export","source","local","readonly","declare","unset","shift","trap","eval","exec","set","read","printf","test","true","false","cd","ls","pwd","mkdir","rm","cp","mv","cat","grep","sed","awk","find","sort","uniq","wc","head","tail","cut","tr","tee","xargs","diff","tar","chmod","chown","ln","curl","wget","ssh","scp","rsync","git","docker","make","python","node","npm","pip"],
          ["PATH","HOME","USER","SHELL","TERM","PWD","EDITOR","LANG","DISPLAY","HOSTNAME","RANDOM","SECONDS","LINENO","FUNCNAME","BASH_SOURCE","PIPESTATUS","REPLY","OPTARG","OPTIND","UID","EUID","SHLVL"],"#"),
        // ── 20. SQL ──
        L("SQL","sql","cylinder",.blue,.data,
          ["SELECT","FROM","WHERE","INSERT","INTO","VALUES","UPDATE","SET","DELETE","CREATE","TABLE","DROP","ALTER","ADD","COLUMN","JOIN","LEFT","RIGHT","INNER","OUTER","FULL","CROSS","ON","AND","OR","NOT","NULL","IS","IN","BETWEEN","LIKE","ORDER","BY","ASC","DESC","GROUP","HAVING","LIMIT","OFFSET","AS","DISTINCT","ALL","UNION","INTERSECT","EXCEPT","EXISTS","CASE","WHEN","THEN","ELSE","END","WITH","RECURSIVE","WINDOW","OVER","PARTITION","RANK","ROW_NUMBER","COUNT","SUM","AVG","MAX","MIN","PRIMARY","KEY","FOREIGN","REFERENCES","UNIQUE","INDEX","CHECK","DEFAULT","CONSTRAINT","CASCADE","VIEW","TRIGGER","FUNCTION","PROCEDURE","RETURN","DECLARE","BEGIN","COMMIT","ROLLBACK","GRANT","REVOKE","EXPLAIN","ANALYZE","COALESCE","NULLIF","CAST","EXTRACT","NOW","CURRENT_TIMESTAMP","GENERATE_SERIES","LATERAL","FILTER"],
          ["INTEGER","INT","SMALLINT","BIGINT","SERIAL","NUMERIC","DECIMAL","REAL","FLOAT","DOUBLE","TEXT","VARCHAR","CHAR","BOOLEAN","BOOL","DATE","TIME","TIMESTAMP","TIMESTAMPTZ","INTERVAL","BLOB","BYTEA","UUID","JSON","JSONB","XML","HSTORE","ARRAY","TSVECTOR","TSQUERY","POINT","INET","MACADDR","ENUM","GEOMETRY","RECORD","VOID"],"--"),
        // ── 21. Lua ──
        L("Lua","lua","l.square",.blue,.scripting,
          ["and","break","do","else","elseif","end","false","for","function","goto","if","in","local","nil","not","or","repeat","return","then","true","until","while","print","require","pairs","ipairs","next","select","unpack","pcall","xpcall","error","assert","type","tostring","tonumber","setmetatable","getmetatable","collectgarbage"],
          ["nil","boolean","number","string","table","function","thread","userdata","io","os","math","coroutine","package","debug","utf8"],"--"),
        // ── 22. R ──
        L("R","r","r.square",.blue,.data,
          ["if","else","for","while","repeat","function","return","next","break","in","TRUE","FALSE","NULL","NA","Inf","NaN","library","require","source","print","cat","paste","sprintf","c","list","data.frame","matrix","array","vector","which","apply","sapply","lapply","tapply","mapply","mutate","filter","select","summarise","arrange","group_by","left_join","right_join","inner_join","pivot_longer","pivot_wider","ggplot","aes","geom_point","geom_line","geom_bar","geom_histogram","geom_boxplot","facet_wrap","labs","theme","ggsave"],
          ["character","numeric","integer","logical","double","complex","list","data.frame","matrix","factor","vector","tibble","environment","formula","function","Date","POSIXct","POSIXlt","difftime","lm","glm"],"#"),
        // ── 23. Scala ──
        L("Scala","scala","s.square",.red,.functional,
          ["def","val","var","lazy","if","else","for","while","do","match","case","return","class","object","trait","extends","with","import","package","new","this","super","yield","try","catch","finally","throw","true","false","null","type","sealed","final","implicit","override","abstract","private","protected","given","using","then","enum","export","extension","opaque","inline"],
          ["String","Int","Long","Double","Float","Boolean","Unit","Any","Nothing","Array","List","Vector","Map","Set","Seq","Option","Some","None","Either","Left","Right","Try","Success","Failure","Future","Promise","Tuple2","Iterator","BigInt","BigDecimal","Duration","IO","ZIO","Task"],"//"),
        // ── 24. Haskell ──
        L("Haskell","hs","h.square",.purple,.functional,
          ["module","where","import","qualified","as","hiding","data","newtype","type","class","instance","deriving","if","then","else","case","of","let","in","do","return","pure","infixl","infixr","forall","family","default","foreign","pattern"],
          ["Int","Integer","Float","Double","Bool","Char","String","Text","ByteString","IO","Maybe","Just","Nothing","Either","Left","Right","Map","Set","HashMap","Vector","Monad","MonadIO","Functor","Applicative","Foldable","Traversable","Semigroup","Monoid","Show","Read","Eq","Ord","STM","TVar","MVar","IORef","Async","ExceptT","ReaderT","StateT","Free"],"--"),
        // ── 25. Elixir ──
        L("Elixir","ex","e.square",.purple,.functional,
          ["def","defp","defmodule","defstruct","defprotocol","defimpl","defmacro","defguard","defdelegate","defexception","do","end","if","else","unless","cond","case","when","fn","raise","rescue","try","catch","after","with","for","in","import","use","alias","require","true","false","nil","and","or","not","receive","send","spawn","self","quote","unquote"],
          ["String","Integer","Float","Atom","List","Map","Tuple","Keyword","Range","Regex","PID","Port","MapSet","Date","Time","DateTime","NaiveDateTime","URI","Agent","GenServer","Supervisor","Task","Stream","Enum","File","IO","Inspect","Module","Process","Registry","Plug","Phoenix.Router","Phoenix.Controller","Phoenix.LiveView","Ecto","Ecto.Changeset","Ecto.Query","Ecto.Repo"],"#"),
        // ── 26. Clojure ──
        L("Clojure","clj","c.square",.green,.functional,
          ["def","defn","defn-","defmacro","defmethod","defmulti","defprotocol","defrecord","deftype","defonce","fn","let","letfn","if","if-let","when","when-let","cond","case","do","loop","recur","for","doseq","dotimes","while","map","filter","reduce","apply","comp","partial","require","use","import","ns","true","false","nil","and","or","not","try","catch","throw","atom","deref","swap!","reset!","assoc","dissoc","merge","update","conj","cons","concat","first","rest","next","take","drop","nth","get","get-in","str","println"],
          ["String","Integer","Long","Double","Boolean","Keyword","Symbol","List","Vector","Map","Set","Atom","Ref","Agent","Var","LazySeq","Range","Regex","File","Exception"],";;"),
        // ── 27. Nim ──
        L("Nim","nim","n.square",.yellow,.systems,
          ["proc","func","method","iterator","template","macro","var","let","const","if","elif","else","when","case","of","for","while","return","import","include","from","export","type","object","enum","concept","ref","ptr","nil","true","false","and","or","not","xor","div","mod","in","notin","is","isnot","discard","echo","result","yield","break","continue","block","defer","raise","try","except","finally","as"],
          ["int","int8","int16","int32","int64","uint","uint8","uint16","uint32","uint64","float","float32","float64","bool","char","string","cstring","pointer","void","auto","seq","array","set","tuple","Table","OrderedTable","HashSet","Deque","Thread","Channel","Future"],"#"),
        // ── 28. Perl ──
        L("Perl","pl","p.square",.cyan,.scripting,
          ["use","my","our","local","sub","if","elsif","else","unless","while","until","for","foreach","do","return","print","say","die","warn","chomp","push","pop","shift","unshift","split","join","sort","reverse","map","grep","open","close","read","eval","BEGIN","END","require","package","bless","ref","defined","undef","scalar","last","next","redo","exit","qw","eq","ne","lt","gt","le","ge","cmp","not","and","or","xor"],
          ["SCALAR","ARRAY","HASH","CODE","REF","GLOB","IO","Regexp"],"#"),
        // ── 29. Zig ──
        L("Zig","zig","z.square",.orange,.systems,
          ["fn","var","const","if","else","while","for","switch","return","break","continue","defer","errdefer","unreachable","pub","comptime","inline","noalias","export","extern","align","volatile","packed","test","try","catch","orelse","async","await","suspend","resume","and","or","true","false","null","undefined","_"],
          ["u8","u16","u32","u64","u128","i8","i16","i32","i64","i128","f16","f32","f64","usize","isize","bool","void","noreturn","type","anyframe","anyopaque","anyerror","anytype","comptime_int","comptime_float"],"//"),
        // ── 30. TOML ──
        L("TOML","toml","doc.text",.orange,.markup,["true","false"],[],"#"),
        // ── 31. YAML ──
        L("YAML","yml","doc.text",.red,.markup,["true","false","null","yes","no","on","off"],[],"#"),
        // ── 32. JSON ──
        L("JSON","json","curlybraces",.yellow,.data,["true","false","null"],[],""),
        // ── 33. XML ──
        L("XML","xml","chevron.left.forwardslash.chevron.right",.orange,.markup,
          ["xml","version","encoding","xmlns","schema","element","attribute","complexType","simpleType","sequence","choice","annotation","restriction","extension","enumeration","pattern","import","include"],
          ["string","integer","decimal","boolean","date","time","dateTime","float","double","anyURI","anyType"],""),
        // ── 34. Markdown ──
        L("Markdown","md","doc.richtext",.blue,.markup,["#","##","###","####","---","```",">"],["**","__","*","_","~~","=="],""),
        // ── 35. LaTeX ──
        L("LaTeX","tex","doc.text",.green,.markup,
          ["documentclass","usepackage","begin","end","section","subsection","title","author","date","maketitle","tableofcontents","chapter","label","ref","cite","caption","footnote","textbf","textit","texttt","emph","newcommand","renewcommand","input","include"],
          ["article","report","book","beamer","figure","table","tabular","equation","align","enumerate","itemize","verbatim","tikzpicture"],"%"),
        // ── 36. Dockerfile ──
        L("Dockerfile","dockerfile","shippingbox",.blue,.devops,
          ["FROM","RUN","CMD","ENTRYPOINT","COPY","ADD","ENV","ARG","EXPOSE","VOLUME","WORKDIR","USER","LABEL","HEALTHCHECK","SHELL","ONBUILD","AS"],[],"#"),
        // ── 37. Makefile ──
        L("Makefile","makefile","hammer",.gray,.devops,
          ["all","clean","install","uninstall","test","build","run","deploy","check","lint","format","dist","release","debug","help","phony","PHONY","include","define","endef","ifdef","ifndef","ifeq","ifneq","else","endif","export","unexport","override","vpath","VPATH"],
          ["CC","CXX","CFLAGS","CXXFLAGS","LDFLAGS","LIBS","INCLUDES","PREFIX","DESTDIR","INSTALL","MKDIR","RM","CP","AR","RANLIB","PKG_CONFIG","SRCS","OBJS","TARGET","BINDIR","LIBDIR","DATADIR","MANDIR","SHELL","MAKE","MAKEFLAGS"],"#"),
        // ── 38. Terraform ──
        L("Terraform","tf","cloud",.purple,.devops,
          ["resource","data","variable","output","locals","module","provider","terraform","required_providers","required_version","backend","provisioner","connection","lifecycle","depends_on","count","for_each","dynamic","content","source","version","type","default","description","sensitive","validation","condition","error_message","create_before_destroy","prevent_destroy","ignore_changes","replace_triggered_by","moved","import","check","assert"],
          ["string","number","bool","list","map","set","object","tuple","any","null","true","false"],"#"),
        // ── 39. GraphQL ──
        L("GraphQL","graphql","network",.pink,.web,
          ["type","input","enum","interface","union","scalar","schema","query","mutation","subscription","fragment","on","extend","implements","directive","repeatable"],
          ["String","Int","Float","Boolean","ID","DateTime","JSON","Upload"],"#"),
        // ── 40. Proto (Protobuf) ──
        L("Protobuf","proto","doc.text",.green,.data,
          ["syntax","package","import","option","message","enum","service","rpc","returns","oneof","map","repeated","optional","required","reserved","extensions","extend","stream"],
          ["double","float","int32","int64","uint32","uint64","sint32","sint64","fixed32","fixed64","sfixed32","sfixed64","bool","string","bytes","any","timestamp","duration","struct","empty","field_mask","wrappers"],"//"),
        // ── 41. Groovy ──
        L("Groovy","groovy","g.square",.green,.scripting,
          ["def","var","if","else","for","while","do","switch","case","default","break","continue","return","class","interface","trait","enum","extends","implements","import","package","new","this","super","try","catch","finally","throw","true","false","null","in","as","instanceof","static","final","abstract","synchronized","println","print","assert"],
          ["String","Integer","Long","Double","Float","Boolean","List","Map","Set","Closure","GString","BigDecimal","BigInteger","Object","Collection","Iterator","Pattern","Matcher","File","InputStream","OutputStream","Binding","Script","GroovyShell","GroovyClassLoader"],"//"),
        // ── 42. OCaml ──
        L("OCaml","ml","o.square",.orange,.functional,
          ["let","in","if","then","else","match","with","when","fun","function","rec","and","or","not","true","false","type","module","struct","sig","end","open","include","val","mutable","ref","begin","do","done","for","while","to","downto","assert","raise","try","exception","external","of","as","lazy","inherit","method","object","class","virtual","private","constraint"],
          ["int","float","bool","char","string","unit","list","array","option","result","ref","exn","format","bytes","nativeint","int32","int64","lazy_t","in_channel","out_channel","Seq","Map","Set","Hashtbl","Buffer","Printf","Scanf","Str","Queue","Stack","Stream","Lwt","Async","Result","Either"],"(*"),
        // ── 43. F# ──
        L("F#","fs","f.square",.blue,.functional,
          ["let","in","if","then","else","elif","match","with","when","fun","function","rec","and","or","not","true","false","type","module","namespace","open","do","done","for","while","to","downto","yield","return","try","with","finally","raise","use","member","static","abstract","default","override","interface","inherit","new","as","upcast","downcast","null","mutable","inline","private","public","internal","async","task","do!","let!","return!","yield!","use!","match!"],
          ["int","float","bool","char","string","unit","byte","sbyte","int16","uint16","int64","uint64","nativeint","single","double","decimal","bigint","list","array","seq","option","voption","result","Map","Set","dict","ResizeArray","Lazy","Async","Task","ValueTask","MailboxProcessor","Agent","Choice","FSharpFunc","FSharpList","FSharpMap","FSharpSet","FSharpOption","FSharpResult"],"//"),
        // ── 44. Erlang ──
        L("Erlang","erl","e.square",.red,.functional,
          ["module","export","import","define","record","include","include_lib","ifdef","ifndef","else","endif","undef","spec","type","opaque","callback","behaviour","if","case","of","end","when","receive","after","fun","try","catch","throw","begin","cond","let","query","true","false","ok","error","undefined","not","and","or","xor","band","bor","bxor","bnot","bsl","bsr","div","rem"],
          ["integer","float","atom","binary","bitstring","boolean","byte","char","function","list","map","mfa","module","neg_integer","no_return","node","none","non_neg_integer","nonempty_string","number","pid","port","pos_integer","reference","string","term","timeout","tuple","arity","gen_server","gen_statem","gen_event","supervisor","application","ets","dets","mnesia","gb_trees","gb_sets","dict","orddict","ordsets","sets","queue","array","digraph","maps","binary","lists","io","file","gen_tcp","gen_udp","ssl","inet","httpc","xmerl","crypto","public_key"],"%%"),
        // ── 45. Julia ──
        L("Julia","jl","j.square",.purple,.data,
          ["function","end","if","elseif","else","for","while","return","begin","let","local","global","const","struct","mutable","abstract","primitive","type","module","import","using","export","macro","quote","do","try","catch","finally","throw","true","false","nothing","missing","in","isa","where","baremodule","continue","break","outer"],
          ["Int","Int8","Int16","Int32","Int64","Int128","UInt","UInt8","UInt16","UInt32","UInt64","UInt128","Float16","Float32","Float64","BigInt","BigFloat","Complex","Rational","Bool","Char","String","Symbol","Tuple","NamedTuple","Pair","Dict","Set","Array","Vector","Matrix","Range","UnitRange","StepRange","Nothing","Missing","Some","Ref","Ptr","Type","DataType","Module","Function","Method","IO","IOBuffer","IOStream","Regex","Match","SubString","Channel","Task","Condition","Lock","ReentrantLock","Semaphore","Timer","Exception","ErrorException","ArgumentError","BoundsError","DimensionMismatch","DomainError","DivideError","InexactError","MethodError","OverflowError","StackOverflowError","TypeError","UndefVarError"],"#"),
        // ── 46. Objective-C ──
        L("Objective-C","m","o.square",.blue,.mobile,
          ["@interface","@implementation","@end","@property","@synthesize","@dynamic","@protocol","@optional","@required","@class","@selector","@encode","@synchronized","@autoreleasepool","@try","@catch","@finally","@throw","@available","@import","if","else","for","while","do","switch","case","break","continue","return","self","super","nil","YES","NO","true","false","NULL","typedef","struct","enum","union","static","extern","const","volatile","inline","register","sizeof","typeof","id","SEL","IMP","Class","BOOL","instancetype","void","int","char","float","double","long","short","unsigned","signed"],
          ["NSObject","NSString","NSMutableString","NSNumber","NSArray","NSMutableArray","NSDictionary","NSMutableDictionary","NSSet","NSMutableSet","NSData","NSMutableData","NSURL","NSURLSession","NSURLRequest","NSURLResponse","NSError","NSException","NSNotification","NSNotificationCenter","NSUserDefaults","NSDate","NSDateFormatter","NSCalendar","NSTimer","NSThread","NSOperationQueue","NSOperation","NSBlockOperation","NSLock","NSCondition","NSFileManager","NSBundle","NSProcessInfo","NSRegularExpression","NSPredicate","NSSortDescriptor","NSCache","NSValueTransformer","UIView","UIViewController","UILabel","UIButton","UITextField","UITextView","UIImageView","UITableView","UICollectionView","UINavigationController","UITabBarController","UIAlertController","UIScrollView","UIStackView","UIWindow","UIApplication","UIScreen","UIColor","UIFont","UIImage","UIStoryboard","UIGestureRecognizer","UITableViewCell","UICollectionViewCell","NSLayoutConstraint"],"//"),
        // ── 47. V (Vlang) ──
        L("V","v","v.square",.blue,.systems,
          ["fn","mut","pub","struct","enum","interface","union","type","import","module","return","if","else","for","in","match","or","and","not","none","true","false","assert","unsafe","defer","go","spawn","shared","rlock","lock","select","static","volatile","extern","asm","as","is","typeof","sizeof","isreftype","dump","println","print","eprintln","panic","exit"],
          ["int","i8","i16","i32","i64","u8","u16","u32","u64","f32","f64","bool","byte","char","rune","string","voidptr","byteptr","charptr","none","any","array","map","chan","thread","IError"],"//"),
        // ── 48. Crystal ──
        L("Crystal","cr","diamond",.gray,.systems,
          ["def","class","struct","module","enum","lib","fun","macro","annotation","if","elsif","else","unless","case","when","in","while","until","loop","do","end","return","next","break","yield","begin","rescue","ensure","raise","require","include","extend","self","super","true","false","nil","typeof","sizeof","instance_sizeof","offsetof","pointerof","as","is_a?","responds_to?","abstract","private","protected","property","getter","setter","record","alias","type","uninitialized","out","spawn","select","forall","with","of"],
          ["String","Int8","Int16","Int32","Int64","Int128","UInt8","UInt16","UInt32","UInt64","UInt128","Float32","Float64","Bool","Char","Symbol","Nil","Void","NoReturn","Array","Hash","Set","Tuple","NamedTuple","Range","Regex","Proc","Pointer","Slice","StaticArray","Deque","Iterator","IO","File","Dir","Path","Socket","HTTP","JSON","YAML","XML","Channel","Fiber","Mutex","WaitGroup","Atomic","Exception","ArgumentError","IndexError","KeyError","TypeCastError","NilAssertionError","DivisionByZeroError","OverflowError"],"#"),
        // ── 49. Svelte ──
        L("Svelte","svelte","globe",.orange,.web,
          ["export","let","const","var","if","else","each","await","then","catch","as","key","html","debug","function","return","import","from","class","new","this","true","false","null","undefined","typeof","instanceof","async","switch","case","break","default","slot","bind","on","use","transition","animate","in","out","style","class"],
          ["onMount","onDestroy","beforeUpdate","afterUpdate","tick","createEventDispatcher","setContext","getContext","getAllContexts","writable","readable","derived","get","tweened","spring","fade","fly","slide","scale","draw","crossfade","flip"],"//"),
        // ── 50. Vue ──
        L("Vue","vue","globe",.green,.web,
          ["export","default","import","from","const","let","var","function","return","if","else","for","while","switch","case","break","continue","try","catch","throw","async","await","true","false","null","undefined","new","this","class","extends","template","script","style","setup","defineProps","defineEmits","defineExpose","defineSlots","withDefaults","ref","reactive","computed","watch","watchEffect","onMounted","onUnmounted","onUpdated","provide","inject","nextTick","defineComponent"],
          ["Ref","ComputedRef","WritableComputedRef","ShallowRef","ShallowReactive","UnwrapRef","PropType","Component","App","Router","Route","Store","Pinia"],"//"),
        // ── 51. Sass/SCSS ──
        L("SCSS","scss","paintbrush",.pink,.web,
          ["@mixin","@include","@extend","@import","@use","@forward","@function","@return","@if","@else","@each","@for","@while","@debug","@warn","@error","@at-root","@content","@media","@supports","@keyframes","@font-face","@layer","@container","@property","!default","!global","!optional"],
          ["$variable","#{interpolation}","map-get","map-set","map-merge","map-remove","map-keys","map-values","map-has-key","list-separator","nth","length","append","join","index","zip","darken","lighten","saturate","desaturate","mix","adjust-hue","complement","invert","grayscale","red","green","blue","alpha","opacity","percentage","round","ceil","floor","abs","min","max","random","unit","unitless","comparable","type-of","inspect","if","unique-id","counter","counters","quote","unquote","str-length","str-insert","str-index","str-slice","to-upper-case","to-lower-case"],"//"),
        // ── 52. Assembly (x86) ──
        L("Assembly","asm","cpu",.gray,.systems,
          ["mov","add","sub","mul","div","inc","dec","cmp","jmp","je","jne","jg","jl","jge","jle","ja","jb","jae","jbe","call","ret","push","pop","lea","nop","int","syscall","xor","and","or","not","shl","shr","sal","sar","rol","ror","test","bt","bts","btr","btc","loop","rep","movs","stos","lods","cmps","scas","enter","leave","hlt","cli","sti","in","out","section","global","extern","db","dw","dd","dq","dt","resb","resw","resd","resq","rest","equ","times","align","org","bits","use16","use32","use64","default","segment","proc","endp","macro","endm","if","else","endif","include","incbin","struc","endstruc"],
          ["rax","rbx","rcx","rdx","rsi","rdi","rbp","rsp","r8","r9","r10","r11","r12","r13","r14","r15","eax","ebx","ecx","edx","esi","edi","ebp","esp","ax","bx","cx","dx","si","di","bp","sp","al","bl","cl","dl","ah","bh","ch","dh","sil","dil","bpl","spl","cs","ds","es","fs","gs","ss","cr0","cr2","cr3","cr4","dr0","dr1","dr2","dr3","dr6","dr7","xmm0","xmm1","xmm2","xmm3","xmm4","xmm5","xmm6","xmm7","ymm0","ymm1","ymm2","ymm3","zmm0","zmm1","zmm2","zmm3","byte","word","dword","qword","tword","oword","yword","zword"],";"),
        // ── 53. WASM/WAT ──
        L("WebAssembly","wat","w.square",.purple,.web,
          ["module","func","param","result","local","global","memory","table","elem","data","start","export","import","type","mut","block","loop","if","then","else","end","br","br_if","br_table","return","call","call_indirect","drop","select","get","set","tee","load","store","offset","align","current_memory","grow_memory","nop","unreachable","i32","i64","f32","f64"],
          ["i32","i64","f32","f64","v128","funcref","externref","anyfunc","i8x16","i16x8","i32x4","i64x2","f32x4","f64x2"],";;"),
        // ── 54. Fortran ──
        L("Fortran","f90","f.square",.green,.data,
          ["program","end","subroutine","function","module","use","implicit","none","intent","in","out","inout","integer","real","double","precision","complex","character","logical","dimension","allocatable","pointer","target","optional","parameter","data","save","common","equivalence","external","intrinsic","if","then","else","elseif","endif","do","enddo","while","select","case","default","endselect","where","elsewhere","endwhere","forall","endforall","block","endblock","associate","endassociate","critical","endcritical","call","return","stop","exit","cycle","continue","goto","print","write","read","open","close","inquire","format","allocate","deallocate","nullify","assigned","contains","recursive","pure","elemental","impure","abstract","interface","endinterface","type","endtype","class","extends","procedure","generic","final","enum","enumerator","endenum","concurrent","sync","all","images","lock","unlock","error","codimension","contiguous","submodule","endsubmodule","change","team","endteam","form","event","post","wait","fail","image","stat","errmsg","source","mold"],
          ["INTEGER","REAL","DOUBLE","COMPLEX","CHARACTER","LOGICAL","DIMENSION","ALLOCATABLE","POINTER","TARGET"],
          "!"),
        // ── 55. COBOL ──
        L("COBOL","cob","c.square",.blue,.systems,
          ["IDENTIFICATION","DIVISION","PROGRAM-ID","ENVIRONMENT","DATA","PROCEDURE","SECTION","WORKING-STORAGE","LOCAL-STORAGE","LINKAGE","FILE","FD","SD","COPY","REPLACE","PERFORM","MOVE","TO","ADD","SUBTRACT","MULTIPLY","DIVIDE","GIVING","COMPUTE","IF","ELSE","END-IF","EVALUATE","WHEN","END-EVALUATE","DISPLAY","ACCEPT","STOP","RUN","GO","CALL","USING","BY","REFERENCE","CONTENT","VALUE","RETURNING","OPEN","CLOSE","READ","WRITE","REWRITE","DELETE","START","STRING","UNSTRING","INSPECT","TALLYING","REPLACING","INITIALIZE","SET","SORT","MERGE","SEARCH","ALL","THRU","THROUGH","UNTIL","VARYING","FROM","AFTER","CONTINUE","EXIT","NOT","AND","OR","EQUAL","GREATER","LESS","THAN","PIC","PICTURE","OCCURS","TIMES","INDEXED","DEPENDING","ON","ASCENDING","DESCENDING","KEY","REDEFINES","RENAMES","FILLER","ZEROS","ZEROES","SPACES","HIGH-VALUES","LOW-VALUES","QUOTE","QUOTES","NULL","NULLS"],
          ["PIC","PICTURE","X","A","9","V","S","P","Z","*","CR","DB","COMP","COMP-1","COMP-2","COMP-3","COMP-5","BINARY","PACKED-DECIMAL","DISPLAY","NATIONAL","POINTER","INDEX","USAGE","OCCURS","SYNC","JUSTIFIED","BLANK","SIGN","LEADING","TRAILING","SEPARATE"],"*>"),
        // ── 56. Ada ──
        L("Ada","adb","a.square",.green,.systems,
          ["abort","abs","abstract","accept","access","aliased","all","and","array","at","begin","body","case","constant","declare","delay","delta","digits","do","else","elsif","end","entry","exception","exit","for","function","generic","goto","if","in","interface","is","limited","loop","mod","new","not","null","of","or","others","out","overriding","package","pragma","private","procedure","protected","raise","range","record","rem","renames","requeue","return","reverse","select","separate","some","subtype","synchronized","tagged","task","terminate","then","type","until","use","when","while","with","xor"],
          ["Boolean","Integer","Natural","Positive","Float","Long_Float","Character","Wide_Character","String","Wide_String","Duration","Time","Address","Storage_Element","Storage_Count","Storage_Offset","Storage_Array","Tag","Exception_Id","Exception_Occurrence","Task_Id"],"--"),
        // ── 57. Prolog ──
        L("Prolog","pl","p.square",.orange,.functional,
          [":-","?-","not","is","mod","div","rem","abs","sign","min","max","succ","plus","true","false","fail","halt","assert","asserta","assertz","retract","retractall","abolish","findall","bagof","setof","forall","aggregate","aggregate_all","between","length","append","member","last","nth0","nth1","msort","sort","predsort","permutation","flatten","select","selectchk","nextto","delete","subtract","intersection","union","maplist","include","exclude","foldl","write","writeln","read","nl","tab","atom","number","integer","float","compound","callable","ground","var","nonvar","atom_string","atom_chars","atom_codes","atom_length","atom_concat","sub_atom","char_type","upcase_atom","downcase_atom","number_chars","number_codes","succ_or_zero","plus_or_minus","char_code","sub_string","string_concat","string_length","string_code","string_codes","format"],
          ["atom","number","integer","float","compound","callable","ground","var","nonvar","list","string","chars","codes","char","term"],"%%"),
        // ── 58. D ──
        L("D","d","d.square",.red,.systems,
          ["abstract","alias","align","asm","assert","auto","body","bool","break","byte","case","cast","catch","char","class","const","continue","dchar","debug","default","delegate","delete","deprecated","do","double","else","enum","export","extern","false","final","finally","float","for","foreach","foreach_reverse","function","goto","idouble","if","ifloat","immutable","import","in","inout","int","interface","invariant","ireal","is","lazy","long","macro","mixin","module","new","nothrow","null","out","override","package","pragma","private","protected","public","pure","real","ref","return","scope","shared","short","static","struct","super","switch","synchronized","template","this","throw","true","try","typeid","typeof","ubyte","uint","ulong","union","unittest","ushort","version","void","wchar","while","with","__FILE__","__LINE__","__MODULE__","__FUNCTION__","__PRETTY_FUNCTION__","__traits"],
          ["string","wstring","dstring","size_t","ptrdiff_t","hash_t","Object","TypeInfo","Exception","Error","Throwable","Range","InputRange","ForwardRange","BidirectionalRange","RandomAccessRange","OutputRange","File","Duration","SysTime","StopWatch","Tid","Thread","Fiber","Mutex","ReadWriteMutex","Condition","Semaphore","Barrier","TaskPool","Variant","Tuple","Nullable","Algebraic","RedBlackTree","Array","DList","SList","HashMap","TreeMap","BitArray","Regex","Appender","OutBuffer"],"//"),
        // ── 59. Solidity ──
        L("Solidity","sol","dollarsign.circle",.gray,.web,
          ["pragma","solidity","import","contract","library","interface","abstract","is","function","modifier","event","error","struct","enum","mapping","constructor","receive","fallback","public","private","internal","external","view","pure","payable","nonpayable","virtual","override","constant","immutable","anonymous","indexed","returns","return","if","else","for","while","do","break","continue","try","catch","revert","require","assert","emit","new","delete","this","super","true","false","type","using","assembly","memory","storage","calldata","msg","block","tx","abi","address","selfdestruct","delegatecall","staticcall","call","send","transfer"],
          ["address","bool","string","bytes","bytes1","bytes2","bytes4","bytes8","bytes16","bytes20","bytes32","int","int8","int16","int32","int64","int128","int256","uint","uint8","uint16","uint32","uint64","uint128","uint256","fixed","ufixed","mapping","array"],"//"),
        // ── 60. Mojo ──
        L("Mojo","mojo","flame",.orange,.systems,
          ["fn","def","struct","trait","var","let","alias","owned","borrowed","inout","raises","capturing","parametric","if","elif","else","for","while","return","pass","break","continue","try","except","finally","raise","with","as","import","from","and","or","not","in","is","True","False","None","self","Self","__init__","__del__","__copyinit__","__moveinit__","__getitem__","__setitem__","__len__","__str__","__repr__","__eq__","__ne__","__lt__","__gt__","__le__","__ge__","__add__","__sub__","__mul__","__truediv__","__floordiv__","__mod__","__pow__","__neg__","__invert__","__and__","__or__","__xor__","__lshift__","__rshift__","__call__","__enter__","__exit__","__iter__","__next__","__contains__","__hash__","print","constrained","always_inline","parameter","register_passable","value","unroll"],
          ["Int","Int8","Int16","Int32","Int64","UInt8","UInt16","UInt32","UInt64","Float16","Float32","Float64","Bool","String","StringLiteral","StringRef","DType","SIMD","Scalar","Tensor","TensorShape","TensorSpec","InlinedFixedVector","DynamicVector","Tuple","Optional","Variant","Pointer","DTypePointer","AnyType","AnyRegType","Movable","Copyable","Stringable","Hashable","EqualityComparable","Intable","Floatable","Boolable","Sized","CollectionElement","KeyElement","RepresentableAsCollection","Indexer","Error","Coroutine","ListLiteral","Slice","Range","NoneType"],"#"),
    ]

    // ══════════════════════════════════════════════════
    // MARK: - Language Detection
    // ══════════════════════════════════════════════════
    static func detect(_ filename: String) -> SynLang? {
        let lower = filename.lowercased()
        // Special filenames first
        if lower == "makefile" || lower == "gnumakefile" { return all.first { $0.name == "Makefile" } }
        if lower == "dockerfile" || lower.hasPrefix("dockerfile") { return all.first { $0.name == "Dockerfile" } }
        if lower == "cmakelists.txt" { return all.first { $0.name == "Makefile" } }
        if lower.hasSuffix(".yml") || lower.hasSuffix(".yaml") { return all.first { $0.name == "YAML" } }
        if lower.hasSuffix(".mojo") || lower.hasSuffix(".🔥") { return all.first { $0.name == "Mojo" } }
        // Extension-based detection
        let ext = (filename as NSString).pathExtension.lowercased()
        let extMap: [String: String] = [
            "swift":"Swift","py":"Python","js":"JavaScript","ts":"TypeScript","jsx":"JSX","tsx":"TSX",
            "html":"HTML","htm":"HTML","css":"CSS","scss":"SCSS","sass":"SCSS",
            "java":"Java","c":"C","h":"C","cpp":"C++","cxx":"C++","cc":"C++","hpp":"C++","hh":"C++",
            "cs":"C#","rs":"Rust","go":"Go","rb":"Ruby","php":"PHP","kt":"Kotlin","kts":"Kotlin",
            "dart":"Dart","sh":"Shell","bash":"Shell","zsh":"Shell","fish":"Shell",
            "sql":"SQL","lua":"Lua","r":"R","scala":"Scala","sc":"Scala",
            "hs":"Haskell","lhs":"Haskell","ex":"Elixir","exs":"Elixir","clj":"Clojure","cljs":"Clojure","cljc":"Clojure",
            "nim":"Nim","nims":"Nim","pl":"Perl","pm":"Perl","zig":"Zig",
            "toml":"TOML","json":"JSON","xml":"XML","svg":"XML","xsd":"XML","xsl":"XML",
            "md":"Markdown","markdown":"Markdown","tex":"LaTeX","sty":"LaTeX","cls":"LaTeX",
            "tf":"Terraform","tfvars":"Terraform","graphql":"GraphQL","gql":"GraphQL",
            "proto":"Protobuf","groovy":"Groovy","gradle":"Groovy",
            "ml":"OCaml","mli":"OCaml","fs":"F#","fsi":"F#","fsx":"F#",
            "erl":"Erlang","hrl":"Erlang","jl":"Julia","m":"Objective-C",
            "v":"V","cr":"Crystal","svelte":"Svelte","vue":"Vue",
            "asm":"Assembly","s":"Assembly","nasm":"Assembly",
            "wat":"WebAssembly","wast":"WebAssembly",
            "f90":"Fortran","f95":"Fortran","f03":"Fortran","f08":"Fortran","f":"Fortran","for":"Fortran",
            "cob":"COBOL","cbl":"COBOL","cpy":"COBOL",
            "adb":"Ada","ads":"Ada","ada":"Ada",
            "pro":"Prolog","d":"D","di":"D","sol":"Solidity","mojo":"Mojo",
        ]
        if let name = extMap[ext] { return all.first { $0.name == name } }
        return nil
    }

    // ══════════════════════════════════════════════════
    // MARK: - Categorized Language Lists
    // ══════════════════════════════════════════════════
    static func byCategory() -> [(category: LangCategory, languages: [SynLang])] {
        LangCategory.allCases.compactMap { cat in
            let langs = all.filter { $0.category == cat }
            return langs.isEmpty ? nil : (category: cat, languages: langs)
        }
    }
    
    static var languageCount: Int { all.count }
    
    static func search(_ query: String) -> [SynLang] {
        guard !query.isEmpty else { return all }
        let q = query.lowercased()
        return all.filter { $0.name.lowercased().contains(q) || $0.ext.lowercased().contains(q) }
    }

    // ══════════════════════════════════════════════════
    // MARK: - Syntax Highlighting Engine
    // ══════════════════════════════════════════════════
    static func highlight(_ code: String, lang: SynLang, isDark: Bool) -> NSAttributedString {
        let r = NSMutableAttributedString(string: code)
        let full = NSRange(location: 0, length: code.utf16.count)
        let defColor: NSColor
        if isDark {
            defColor = NSColor(red:0.91,green:0.894,blue:0.941,alpha:1)
        } else {
            defColor = NSColor(red:0.11,green:0.11,blue:0.145,alpha:1)
        }
        r.addAttribute(.foregroundColor, value: defColor, range: full)
        r.addAttribute(.font, value: NSFont.monospacedSystemFont(ofSize: 13.5, weight: .regular), range: full)
        
        // Colors
        let kC: NSColor
        if isDark {
            kC = NSColor(red:0.655,green:0.545,blue:0.98,alpha:1)
        } else {
            kC = NSColor(red:0.42,green:0.33,blue:0.82,alpha:1)
        }
        let tC: NSColor
        if isDark {
            tC = NSColor(red:0.404,green:0.91,blue:0.976,alpha:1)
        } else {
            tC = NSColor(red:0.15,green:0.58,blue:0.67,alpha:1)
        }
        let sC: NSColor
        if isDark {
            sC = NSColor(red:0.298,green:0.851,blue:0.392,alpha:1)
        } else {
            sC = NSColor(red:0.16,green:0.59,blue:0.24,alpha:1)
        }
        let cC: NSColor
        if isDark {
            cC = NSColor(red:0.545,green:0.522,blue:0.627,alpha:1)
        } else {
            cC = NSColor(red:0.56,green:0.56,blue:0.62,alpha:1)
        }
        let nC: NSColor
        if isDark {
            nC = NSColor(red:0.976,green:0.859,blue:0.369,alpha:1)
        } else {
            nC = NSColor(red:0.71,green:0.59,blue:0.08,alpha:1)
        }
        let opC: NSColor
        if isDark {
            opC = NSColor(red:0.96,green:0.651,blue:0.137,alpha:1)
        } else {
            opC = NSColor(red:0.82,green:0.545,blue:0.08,alpha:1)
        }
        
        func applyColor(_ loc: Int, _ len: Int, _ c: NSColor) {
            guard loc >= 0, len > 0, loc + len <= code.utf16.count else { return }
            r.addAttribute(.foregroundColor, value: c, range: NSRange(location: loc, length: len))
        }
        
        // Process line by line
        var offset = 0
        let lines = code.split(separator: "\n", omittingEmptySubsequences: false).map(String.init)
        var inBlockComment = false
        var _ = false // inMultilineString reserved for future
        
        for line in lines {
            let trimmed = line.trimmingCharacters(in: .whitespaces)
            
            // Block comment tracking
            if !lang.slc.isEmpty {
                if trimmed.hasPrefix("/*") { inBlockComment = true }
                if inBlockComment {
                    applyColor(offset, line.count, cC)
                    if trimmed.contains("*/") { inBlockComment = false }
                    offset += line.count + 1
                    continue
                }
            }
            
            // Single-line comments
            if !lang.slc.isEmpty && trimmed.hasPrefix(lang.slc) {
                applyColor(offset, line.count, cC)
                offset += line.count + 1
                continue
            }
            
            // Strings (simple detection for "..." and '...')
            var inString = false
            var stringChar: Character = "\""
            var stringStart = 0
            
            var i = 0
            let chars = Array(line)
            while i < chars.count {
                if !inString {
                    if chars[i] == "\"" || chars[i] == "'" || chars[i] == "`" {
                        inString = true; stringChar = chars[i]; stringStart = i
                    }
                } else {
                    if chars[i] == stringChar && (i == 0 || chars[i-1] != "\\") {
                        applyColor(offset + stringStart, i - stringStart + 1, sC)
                        inString = false
                    }
                }
                i += 1
            }
            if inString { applyColor(offset + stringStart, chars.count - stringStart, sC) }
            
            // Inline comments (after code)
            if !lang.slc.isEmpty {
                if let commentRange = line.range(of: lang.slc) {
                    let commentOffset = line.distance(from: line.startIndex, to: commentRange.lowerBound)
                    applyColor(offset + commentOffset, line.count - commentOffset, cC)
                }
            }
            
            // Keywords and types
            var wordStart = -1
            for (j, ch) in line.enumerated() {
                let isWordChar = ch.isLetter || ch == "_" || ch == "@" || ch == "#" || ch == "!" || ch == "?" || (wordStart >= 0 && ch.isNumber) || (wordStart >= 0 && ch == "-" && lang.name == "CSS")
                if isWordChar {
                    if wordStart < 0 { wordStart = j }
                } else {
                    if wordStart >= 0 {
                        let word = String(line[line.index(line.startIndex, offsetBy: wordStart)..<line.index(line.startIndex, offsetBy: j)])
                        if lang.kw.contains(word) { applyColor(offset + wordStart, j - wordStart, kC) }
                        else if lang.types.contains(word) { applyColor(offset + wordStart, j - wordStart, tC) }
                        wordStart = -1
                    }
                    // Numbers
                    if ch.isNumber && (j == 0 || !chars[j-1].isLetter) {
                        applyColor(offset + j, 1, nC)
                    }
                    // Operators
                    if "=+-*/<>&|^~!%".contains(ch) {
                        applyColor(offset + j, 1, opC)
                    }
                }
            }
            // Word at end of line
            if wordStart >= 0 {
                let word = String(line[line.index(line.startIndex, offsetBy: wordStart)...])
                if lang.kw.contains(word) { applyColor(offset + wordStart, line.count - wordStart, kC) }
                else if lang.types.contains(word) { applyColor(offset + wordStart, line.count - wordStart, tC) }
            }
            
            offset += line.count + 1
        }
        return r
    }
}

// ══════════════════════════════════════════════════
// MARK: - NSTextView Syntax Editor
// ══════════════════════════════════════════════════
struct SyntaxTextView: NSViewRepresentable {
    @Binding var text: String
    var lang: SynLang?
    var isDark: Bool
    var bgColor: NSColor
    var fontSize: CGFloat = 13.5
    var showLineNumbers: Bool = true

    func makeNSView(context: Context) -> NSView {
        let wrapper = NSView()
        wrapper.wantsLayer = true
        
        // Background color — hardcoded to avoid SwiftUI Color conversion issues
        let bg: NSColor = isDark
            ? NSColor(red:0.035, green:0.031, blue:0.067, alpha:1)
            : NSColor.white
        
        // Create scroll view + text view using Apple's factory
        let sv = NSTextView.scrollableTextView()
        sv.hasVerticalScroller = true
        sv.hasHorizontalScroller = false
        sv.borderType = .noBorder
        sv.drawsBackground = true
        sv.backgroundColor = bg
        sv.translatesAutoresizingMaskIntoConstraints = false
        
        guard let tv = sv.documentView as? NSTextView else {
            wrapper.addSubview(sv)
            return wrapper
        }
        
        let font = NSFont.monospacedSystemFont(ofSize: fontSize, weight: .regular)
        tv.font = font
        tv.backgroundColor = bg
        tv.textColor = isDark
            ? NSColor(red:0.91, green:0.89, blue:0.94, alpha:1)
            : NSColor(red:0.11, green:0.11, blue:0.145, alpha:1)
        tv.insertionPointColor = NSColor(red:0.96, green:0.65, blue:0.14, alpha:1)
        tv.isEditable = true
        tv.isSelectable = true
        tv.allowsUndo = true
        tv.isRichText = false
        tv.usesFindBar = true
        
        // Typography
        let para = NSMutableParagraphStyle()
        let lh = font.boundingRectForFont.height * 1.4
        para.minimumLineHeight = lh; para.maximumLineHeight = lh
        tv.defaultParagraphStyle = para
        tv.textContainerInset = NSSize(width: 8, height: 8)
        
        // Disable auto-corrections
        tv.isAutomaticQuoteSubstitutionEnabled = false
        tv.isAutomaticDashSubstitutionEnabled = false
        tv.isAutomaticTextReplacementEnabled = false
        tv.isAutomaticSpellingCorrectionEnabled = false
        tv.smartInsertDeleteEnabled = false
        
        tv.delegate = context.coordinator
        tv.string = text
        context.coordinator.lastText = text
        context.coordinator.scrollView = sv
        
        // Layout: gutter on left, scroll view fills rest
        let gutterW: CGFloat = showLineNumbers ? 40 : 0
        
        if showLineNumbers {
            let gutter = LineNumberGutter(textView: tv, fontSize: fontSize, isDark: isDark)
            gutter.translatesAutoresizingMaskIntoConstraints = false
            wrapper.addSubview(gutter)
            context.coordinator.gutter = gutter
            
            wrapper.addSubview(sv)
            
            NSLayoutConstraint.activate([
                gutter.leadingAnchor.constraint(equalTo: wrapper.leadingAnchor),
                gutter.topAnchor.constraint(equalTo: wrapper.topAnchor),
                gutter.bottomAnchor.constraint(equalTo: wrapper.bottomAnchor),
                gutter.widthAnchor.constraint(equalToConstant: gutterW),
                
                sv.leadingAnchor.constraint(equalTo: gutter.trailingAnchor),
                sv.trailingAnchor.constraint(equalTo: wrapper.trailingAnchor),
                sv.topAnchor.constraint(equalTo: wrapper.topAnchor),
                sv.bottomAnchor.constraint(equalTo: wrapper.bottomAnchor),
            ])
        } else {
            wrapper.addSubview(sv)
            NSLayoutConstraint.activate([
                sv.leadingAnchor.constraint(equalTo: wrapper.leadingAnchor),
                sv.trailingAnchor.constraint(equalTo: wrapper.trailingAnchor),
                sv.topAnchor.constraint(equalTo: wrapper.topAnchor),
                sv.bottomAnchor.constraint(equalTo: wrapper.bottomAnchor),
            ])
        }
        
        // Scroll notifications for gutter refresh
        tv.postsFrameChangedNotifications = true
        sv.contentView.postsBoundsChangedNotifications = true
        NotificationCenter.default.addObserver(
            context.coordinator,
            selector: #selector(Coordinator.viewDidChange(_:)),
            name: NSView.boundsDidChangeNotification,
            object: sv.contentView
        )
        NotificationCenter.default.addObserver(
            context.coordinator,
            selector: #selector(Coordinator.viewDidChange(_:)),
            name: NSView.frameDidChangeNotification,
            object: tv
        )
        
        // Deferred highlight
        DispatchQueue.main.asyncAfter(deadline: .now() + 0.15) {
            self.doHighlight(tv)
            context.coordinator.gutter?.needsDisplay = true
        }
        
        return wrapper
    }

    func updateNSView(_ wrapper: NSView, context: Context) {
        let coord = context.coordinator
        guard let sv = coord.scrollView, let tv = sv.documentView as? NSTextView else { return }
        
        let bg: NSColor = isDark
            ? NSColor(red:0.035, green:0.031, blue:0.067, alpha:1)
            : NSColor.white
        
        // Sync text when binding changed externally
        if tv.string != text && !coord.isTyping {
            coord.isSyncing = true
            tv.string = text
            coord.lastText = text
            doHighlight(tv)
            coord.isSyncing = false
        }
        
        // Sync appearance
        let font = NSFont.monospacedSystemFont(ofSize: fontSize, weight: .regular)
        tv.backgroundColor = bg
        tv.font = font
        tv.textColor = isDark
            ? NSColor(red:0.91, green:0.89, blue:0.94, alpha:1)
            : NSColor(red:0.11, green:0.11, blue:0.145, alpha:1)
        sv.backgroundColor = bg
        
        if let g = coord.gutter {
            g.isDark = isDark
            g.fontSize = fontSize
            g.needsDisplay = true
        }
    }

    func doHighlight(_ tv: NSTextView) {
        guard let lang = lang, let ts = tv.textStorage, !tv.string.isEmpty else { return }
        let font = NSFont.monospacedSystemFont(ofSize: fontSize, weight: .regular)
        let hl = Syn.highlight(tv.string, lang: lang, isDark: isDark)
        let ranges = tv.selectedRanges
        ts.beginEditing()
        ts.setAttributedString(hl)
        ts.addAttribute(.font, value: font, range: NSRange(location: 0, length: ts.length))
        // Re-apply paragraph style
        let para = NSMutableParagraphStyle()
        let lh = font.boundingRectForFont.height * 1.4
        para.minimumLineHeight = lh; para.maximumLineHeight = lh
        ts.addAttribute(.paragraphStyle, value: para, range: NSRange(location: 0, length: ts.length))
        ts.endEditing()
        if !ranges.isEmpty {
            let maxLen = (tv.string as NSString).length
            let safe = ranges.compactMap { rv -> NSValue? in
                let r = rv.rangeValue
                return r.location <= maxLen ? NSValue(range: NSRange(location: min(r.location, maxLen), length: 0)) : nil
            }
            if !safe.isEmpty { tv.selectedRanges = safe }
        }
    }

    func makeCoordinator() -> Coordinator { Coordinator(self) }

    class Coordinator: NSObject, NSTextViewDelegate {
        var parent: SyntaxTextView
        var gutter: LineNumberGutter?
        var scrollView: NSScrollView?
        var isSyncing = false
        var isTyping = false
        var lastText = ""
        var hlTimer: Timer?
        
        init(_ p: SyntaxTextView) { self.parent = p }
        
        func textDidChange(_ n: Notification) {
            guard !isSyncing, let tv = n.object as? NSTextView else { return }
            isTyping = true
            parent.text = tv.string
            lastText = tv.string
            isTyping = false
            gutter?.needsDisplay = true
            
            hlTimer?.invalidate()
            hlTimer = Timer.scheduledTimer(withTimeInterval: 0.3, repeats: false) { [weak self] _ in
                guard let self = self else { return }
                self.isSyncing = true
                self.parent.doHighlight(tv)
                self.isSyncing = false
            }
        }
        
        @objc func viewDidChange(_ n: Notification) {
            gutter?.needsDisplay = true
        }
    }
}

