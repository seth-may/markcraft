.PHONY: all build test clean lint docker deploy help

# Variables
APP_NAME    := markcraft-server
VERSION     := $(shell git describe --tags --always)
BUILD_DIR   := ./build
GO_FILES    := $(shell find . -name '*.go' -not -path "./vendor/*")
LDFLAGS     := -ldflags "-w -s -X main.version=$(VERSION)"

# Colors
GREEN  := \033[0;32m
YELLOW := \033[0;33m
RESET  := \033[0m

## help: Show this help message
help:
	@echo "$(GREEN)$(APP_NAME) v$(VERSION)$(RESET)"
	@echo "Available targets:"
	@sed -n 's/^## //p' $(MAKEFILE_LIST) | column -t -s ':'

## all: Build and test
all: lint test build

## build: Compile the binary
build: $(BUILD_DIR)
	@echo "$(GREEN)Building $(APP_NAME)...$(RESET)"
	CGO_ENABLED=0 go build $(LDFLAGS) -o $(BUILD_DIR)/$(APP_NAME) ./cmd/server
	@echo "$(GREEN)Done: $(BUILD_DIR)/$(APP_NAME)$(RESET)"

$(BUILD_DIR):
	@mkdir -p $(BUILD_DIR)

## test: Run unit tests with coverage
test:
	@echo "$(YELLOW)Running tests...$(RESET)"
	go test -v -race -coverprofile=coverage.out ./...
	go tool cover -html=coverage.out -o coverage.html
	@echo "$(GREEN)Coverage report: coverage.html$(RESET)"

## lint: Run linters
lint:
	golangci-lint run ./...
	staticcheck ./...

## clean: Remove build artifacts
clean:
	rm -rf $(BUILD_DIR) coverage.out coverage.html
	go clean -cache

## docker: Build Docker image
docker:
	docker build -t $(APP_NAME):$(VERSION) \
		--build-arg VERSION=$(VERSION) .
	docker tag $(APP_NAME):$(VERSION) $(APP_NAME):latest

## deploy: Deploy to production
deploy: docker
	@echo "$(YELLOW)Deploying $(VERSION)...$(RESET)"
	kubectl set image deployment/$(APP_NAME) \
		$(APP_NAME)=$(APP_NAME):$(VERSION) --record
	kubectl rollout status deployment/$(APP_NAME)
