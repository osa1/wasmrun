pub mod builder;
pub mod elements;
mod io;

pub use elements::{
    deserialize_buffer, deserialize_file, peek_size, serialize, serialize_to_file,
    Error as SerializationError,
};
