# Multi-stage build for a Go microservice
# Stage 1: Build
FROM golang:1.22-alpine AS builder

RUN apk add --no-cache git ca-certificates tzdata
WORKDIR /build

# Cache dependencies
COPY go.mod go.sum ./
RUN go mod download && go mod verify

# Build binary
COPY . .
RUN CGO_ENABLED=0 GOOS=linux GOARCH=amd64 \
    go build -ldflags="-w -s -X main.version=${VERSION}" \
    -o /build/server ./cmd/server

# Stage 2: Test
FROM builder AS tester
RUN go test -v -race -coverprofile=coverage.out ./...
RUN go tool cover -func=coverage.out

# Stage 3: Production
FROM scratch

LABEL maintainer="team@markcraft.fr"
LABEL org.opencontainers.image.source="https://github.com/markcraft-app"

COPY --from=builder /usr/share/zoneinfo /usr/share/zoneinfo
COPY --from=builder /etc/ssl/certs/ca-certificates.crt /etc/ssl/certs/
COPY --from=builder /build/server /server
COPY --from=builder /build/configs /configs

ENV TZ=Europe/Paris
ENV APP_ENV=production
ENV PORT=8080

EXPOSE 8080
HEALTHCHECK --interval=30s --timeout=3s --retries=3 \
    CMD ["/server", "healthcheck"]

USER 1000:1000
ENTRYPOINT ["/server"]
CMD ["serve", "--config", "/configs/prod.yaml"]
