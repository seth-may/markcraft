// V language: HTTP server with concurrent handlers
module main

import net.http
import json
import time
import sync
import os

struct App {
    mut:
    users   map[string]User
    mu      sync.RwMutex
    started time.Time
}

struct User {
    id       string
    name     string
    email    string
    created  string
}

struct ApiResponse {
    status  int
    message string
    data    string
}

fn new_app() &App {
    return &App{
        users: map[string]User{}
        started: time.now()
    }
}

fn (mut app App) add_user(user User) {
    app.mu.@lock()
    defer { app.mu.unlock() }
    app.users[user.id] = user
}

fn (app &App) get_user(id string) ?User {
    app.mu.rlock()
    defer { app.mu.runlock() }
    return app.users[id] or { return none }
}

fn (app &App) handle_status(mut req http.Request) http.Response {
    uptime := time.since(app.started)
    body := json.encode(ApiResponse{
        status: 200
        message: 'ok'
        data: 'uptime: ${uptime}'
    })
    return http.Response{
        status: .ok
        body: body
        header: http.new_header_from_map({'Content-Type': 'application/json'})
    }
}

fn (mut app App) handle_create_user(mut req http.Request) http.Response {
    user := json.decode(User, req.body) or {
        return http.Response{ status: .bad_request, body: 'Invalid JSON' }
    }
    app.add_user(user)
    return http.Response{
        status: .created
        body: json.encode(user)
    }
}

fn main() {
    port := os.getenv_opt('PORT') or { '8080' }
    mut app := new_app()
    
    mut server := http.Server{
        handler: fn [mut app] (mut req http.Request) http.Response {
            match req.url {
                '/status' { return app.handle_status(mut req) }
                '/users'  { return app.handle_create_user(mut req) }
                else      { return http.Response{ status: .not_found } }
            }
        }
    }
    
    println('MarkCraft API running on :${port}')
    server.listen_and_serve(':${port}')
}
