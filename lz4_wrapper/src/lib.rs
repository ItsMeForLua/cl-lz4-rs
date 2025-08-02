use lz4_flex::block::{compress_into, decompress_into, get_maximum_output_size};
use std::slice;

#[unsafe(no_mangle)]
pub extern "C" fn lz4_compress_bound(input_size: usize) -> usize {
    get_maximum_output_size(input_size)
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn lz4_compress_into(
    src: *const u8,
    src_len: usize,
    dest: *mut u8,
    dest_len: usize,
) -> i32 {
    unsafe {
        let src_slice = slice::from_raw_parts(src, src_len);
        let dest_slice = slice::from_raw_parts_mut(dest, dest_len);

        match compress_into(src_slice, dest_slice) {
            Ok(bytes_written) => bytes_written as i32,
            Err(_) => -1,
        }
    }
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn lz4_decompress_into(
    src: *const u8,
    src_len: usize,
    dest: *mut u8,
    dest_len: usize,
) -> i32 {
    unsafe {
        let src_slice = slice::from_raw_parts(src, src_len);
        let dest_slice = slice::from_raw_parts_mut(dest, dest_len);

        match decompress_into(src_slice, dest_slice) {
            Ok(bytes_written) => bytes_written as i32,
            Err(_) => -1,
        }
    }
}
