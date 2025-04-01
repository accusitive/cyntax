// ===========================================================
// Complex Preprocessor Example for Ifdef Tree Testing
// ===========================================================

// --- Top Level Platform Selection ---
#ifdef PLATFORM_WINDOWS
#define OS_NAME "Windows"
#define HAS_WIN32_API 1

// --- Windows Architecture ---
#ifdef ARCH_X64
#define ARCH_NAME "x64"
#define POINTER_SIZE 8

// --- Windows x64 Features ---
#ifdef FEATURE_AVX2
#define SIMD_LEVEL 2
#ifdef BUILD_DEBUG
int win_x64_avx2_debug = 1;
#else                       // NOT BUILD_DEBUG
int win_x64_avx2_release = 1;
#endif                      // BUILD_DEBUG
#elif defined(FEATURE_SSE4) // Use defined() in elif
#define SIMD_LEVEL 1
int win_x64_sse4 = 1;
#else // No SIMD features specified for Win x64
#define SIMD_LEVEL 0
int win_x64_nosimd = 1;
#endif // FEATURE_AVX2 / FEATURE_SSE4

#elif defined(ARCH_ARM64) // Use defined() in elif
#define ARCH_NAME "ARM64"
#define POINTER_SIZE 8
#define HAS_NEON 1 // Assume NEON for ARM64

#ifndef FEATURE_LEGACY_UI // Test #ifndef
// Modern UI Path for Win ARM64
#define UI_MODE "Modern"
#ifdef FEATURE_TOUCH_INPUT
int win_arm64_modern_touch = 1;
#else
int win_arm64_modern_notouch = 1;
#endif // FEATURE_TOUCH_INPUT
#else  // FEATURE_LEGACY_UI is defined
// Legacy UI Path for Win ARM64
#define UI_MODE "Legacy"
int win_arm64_legacy_ui = 1;
#endif // FEATURE_LEGACY_UI

#else                            // Default or unsupported Windows Arch
#define ARCH_NAME "Unknown/x86?" // Maybe fallback or error
#define POINTER_SIZE 4           // Guessing 32-bit if not specified
#warning "Unknown or unsupported Windows architecture specified!"
int win_unknown_arch = 1;
#endif // ARCH_X64 / ARCH_ARM64

#elif defined(PLATFORM_LINUX) // Use defined() in elif
#define OS_NAME "Linux"
#define HAS_POSIX_API 1

// --- Linux Build Type ---
#ifdef BUILD_RELEASE
#define BUILD_TYPE "Release"
#define OPTIMIZATION_LEVEL 3

// --- Linux Release Features ---
#ifdef FEATURE_LTO // Link Time Optimization
#define USE_LTO 1
int linux_release_lto = 1;
#else
#define USE_LTO 0
int linux_release_nolto = 1;
// Nested check within Release/NoLTO
#ifndef CONFIG_FAST_MATH // Check if fast math is disabled
#define ENABLE_STRICT_FP 1
int linux_release_nolto_strictfp = 1;
#endif // CONFIG_FAST_MATH
#endif // FEATURE_LTO

#elif defined(BUILD_DEBUG) // Use defined() in elif
#define BUILD_TYPE "Debug"
#define OPTIMIZATION_LEVEL 0
#define ENABLE_ASSERTS 1
int linux_debug_build = 1;

// --- Debug specific sub-options ---
#ifdef FEATURE_MEM_DEBUG
#define MEMORY_TOOL "ValgrindHints"
int linux_debug_memdebug = 1;
#elif defined(FEATURE_ADDR_SANITIZE)
#define MEMORY_TOOL "ASan"
int linux_debug_asan = 1;
#else
#define MEMORY_TOOL "None"
int linux_debug_nomemtool = 1;
#endif // FEATURE_MEM_DEBUG / FEATURE_ADDR_SANITIZE

#else // Default build type for Linux (e.g., Profile?)
#define BUILD_TYPE "Profile"
#define OPTIMIZATION_LEVEL 2
int linux_profile_build = 1;
#endif          // BUILD_RELEASE / BUILD_DEBUG

// --- Common Linux Setting (after build type) ---
#ifdef ARCH_ARM // Simple ARM check for Linux
#define LINUX_TARGET "ARM"
int linux_arm_target = 1;
#else
#define LINUX_TARGET "x86_family"
int linux_x86_target = 1;
#endif // ARCH_ARM

#elif defined(PLATFORM_MACOS) // Use defined() in elif
#define OS_NAME "MacOS"
#define HAS_POSIX_API 1 // MacOS is mostly POSIX compliant
#define HAS_COCOA_API 1

#ifdef FEATURE_METAL // Graphics API choice
#define GFX_API "Metal"
int macos_metal = 1;
#else
#define GFX_API "OpenGL" // Default if Metal not specified
int macos_opengl = 1;
#endif                   // FEATURE_METAL

#else // --- Fallback for Unknown Platform ---
#define OS_NAME "Unknown"
#warning "Target platform not specified or recognized!"

#ifdef FEATURE_GENERIC_FALLBACK
// Specific code path if a generic fallback is explicitly enabled
#define CONFIG_TYPE "GenericFallback"
int generic_fallback_enabled = 1;
// Test an empty block
#ifdef INNER_EMPTY_TEST
#endif // INNER_EMPTY_TEST
#else
// Default behavior for unknown platform without generic fallback flag
#define CONFIG_TYPE "GenericError"
int generic_error_path = 1;
#error "Cannot compile for unknown platform without FEATURE_GENERIC_FALLBACK."
#endif // FEATURE_GENERIC_FALLBACK

#endif // PLATFORM_WINDOWS / PLATFORM_LINUX / PLATFORM_MACOS

// --- Testing #undef and re-evaluation ---
#define TEMPORARY_FEATURE 1

#ifdef TEMPORARY_FEATURE
#define PART_A_DEFINED 1
const char *part_a_status = "Part A was processed";
#undef TEMPORARY_FEATURE // << Critical: Undefine the macro
#endif                   // TEMPORARY_FEATURE

// This block should NOT execute its #ifdef part
#ifdef TEMPORARY_FEATURE
#error "TEMPORARY_FEATURE should be undefined here!"
int part_b_error = 1;
#else // This #else block SHOULD execute
#define PART_B_DEFINED 1
const char *part_b_status = "Part B processed correctly (TEMPORARY_FEATURE is undefined)";
#ifdef PART_A_DEFINED // Check if Part A *was* defined before the undef
const char *part_b_extra = "Part A was also defined previously.";
int part_b_sees_part_a = 1;
#endif                // PART_A_DEFINED
#endif                // TEMPORARY_FEATURE

// This block SHOULD execute its #ifndef part
#ifndef TEMPORARY_FEATURE
#define PART_C_DEFINED 1
const char *part_c_status = "Part C processed correctly (TEMPORARY_FEATURE is undefined)";
int part_c_check = 1;
#else
#error "TEMPORARY_FEATURE should be undefined here (Part C)!"
int part_c_error = 1;
#endif // TEMPORARY_FEATURE

// --- Final Configuration Summary (Example) ---
#if defined(OS_NAME) && defined(ARCH_NAME)
const char *os_info = OS_NAME;
const char *arch_info = ARCH_NAME;
int config_summary_available = 1;
#elif defined(OS_NAME) && defined(BUILD_TYPE) // e.g. Linux case
const char *os_info = OS_NAME;
const char *build_info = BUILD_TYPE;
int config_summary_available = 2; // Different value
#else
// Fallback summary or indicator
int config_summary_available = 0;
#endif // Combined check

// ===========================================================
// End of Complex Preprocessor Example
// ===========================================================

// Dummy main to make it compilable
int main()
{
#ifdef config_summary_available
    // reference some variables to avoid unused warnings potentially
    (void)os_info;
#ifdef arch_info
    (void)arch_info;
#endif
#ifdef build_info
    (void)build_info;
#endif
#ifdef part_a_status
    (void)part_a_status;
#endif
#ifdef part_b_status
    (void)part_b_status;
#endif
#ifdef part_c_status
    (void)part_c_status;
#endif
#endif
    return 0;
}