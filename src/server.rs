use crate::compiler;
use anyhow::Result;
use axum::http::StatusCode;
use axum::{
    routing::{get, post},
    Json, Router,
};
use serde::{Deserialize, Serialize};
use std::env;
use std::net::SocketAddr;
use tower_http::cors::CorsLayer;

/// Runs server for playground. Receives code and compiles it to JS
#[tokio::main]
pub async fn run_server() -> Result<()> {
    tracing_subscriber::fmt::init();

    let app = Router::new()
        .route("/", get(root))
        .route("/compile", post(compile))
        .layer(CorsLayer::permissive());

    let port = match env::var("PORT") {
        Ok(port) => port.parse()?,
        Err(_) => 3000,
    };
    let addr = SocketAddr::from(([127, 0, 0, 1], port));
    tracing::info!("listening on {}", addr);
    axum::Server::bind(&addr)
        .serve(app.into_make_service())
        .await
        .unwrap();

    Ok(())
}

async fn root() -> &'static str {
    "Hello world!"
}

#[derive(Deserialize)]
struct CompileRequest {
    code: String,
}

#[derive(Serialize)]
struct CompileResponse {
    code: String,
    errors: String,
}

async fn compile(Json(payload): Json<CompileRequest>) -> Result<Json<CompileResponse>, StatusCode> {
    let mut output = Vec::new();

    let errors = if let Err(err) = compiler::compile(&payload.code, &mut output) {
        err.to_string()
    } else {
        String::new()
    };

    Ok(Json(CompileResponse {
        code: String::from_utf8(output).map_err(|_| StatusCode::INTERNAL_SERVER_ERROR)?,
        errors,
    }))
}
