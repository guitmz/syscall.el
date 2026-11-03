# Changelog

## 0.1.0

First release. Needs Emacs 29.1 or newer.

- Offline cache of every architecture from api.syscall.sh, with ETag updates, retries and a staging step so a failed update never breaks what you have
- `syscall-lookup` and `syscall-lookup-number` with annotated completion and a detail buffer
- `syscall-list` with a live filter that works like the one on syscall.sh and matches families and kconfig options too
- Family filter in the list, so you can show only `epoll` or `io_uring` calls
- `syscall-compare`, `syscall-conventions-table` and `syscall-diff`, all offline
- Any scanned kernel release, or all of them at once
- `syscall-insert-asm-stub` for x86, ARM, AArch64, riscv64 and MIPS
- `syscall-eldoc-mode` for `SYS_name` and `__NR_name` in C and assembly
