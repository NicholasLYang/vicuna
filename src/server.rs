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
pub struct CompileResponse {
    pub output_code: Option<String>,
    pub cst: String,
    pub errors: Option<String>,
}

async fn compile(Json(payload): Json<CompileRequest>) -> Result<Json<CompileResponse>, StatusCode> {
    let cst = compiler::get_cst(&payload.code)
        .map_or_else(|| String::new(), |cst| cst.root_node().to_sexp());
    match compiler::compile(&payload.code) {
        Ok(compile_response) => Ok(Json(CompileResponse {
            output_code: Some(compile_response),
            cst,
            errors: None,
        })),
        Err(err) => Ok(Json(CompileResponse {
            output_code: None,
            cst,
            errors: Some(err.to_string()),
        })),
    }
}
