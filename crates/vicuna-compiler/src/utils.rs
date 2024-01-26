use camino::{Utf8Path, Utf8PathBuf};

pub fn clean(path: &Utf8Path) -> Utf8PathBuf {
    clean_path::clean(path).try_into().unwrap()
}
