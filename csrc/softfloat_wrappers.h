/*
 * Wrappers for softfloat functions. All functions beginning with the "hs_" prefix
 * have the same signature as their softfloat counterparts, except that all floatXX_t
 * types are changed to uintXX_t. This makes them more immediately compatible with
 * the Haskell FFI.
 */
#include <softfloat.h>

uint16_t hs_ui32_to_f16( uint32_t );
uint32_t hs_ui32_to_f32( uint32_t );
uint64_t hs_ui32_to_f64( uint32_t );
uint16_t hs_ui64_to_f16( uint64_t );
uint32_t hs_ui64_to_f32( uint64_t );
uint64_t hs_ui64_to_f64( uint64_t );
uint16_t hs_i32_to_f16( int32_t );
uint32_t hs_i32_to_f32( int32_t );
uint64_t hs_i32_to_f64( int32_t );
uint16_t hs_i64_to_f16( int64_t );
uint32_t hs_i64_to_f32( int64_t );
uint64_t hs_i64_to_f64( int64_t );

uint32_t hs_f16_to_ui32( uint16_t, uint8_t, bool );
uint64_t hs_f16_to_ui64( uint16_t, uint8_t, bool );
int32_t hs_f16_to_i32( uint16_t, uint8_t, bool );
int64_t hs_f16_to_i64( uint16_t, uint8_t, bool );
uint32_t hs_f16_to_f32( uint16_t );
uint64_t hs_f16_to_f64( uint16_t );

uint16_t hs_f16_roundToInt( uint16_t, uint8_t, bool );
uint16_t hs_f16_add( uint16_t, uint16_t );
uint16_t hs_f16_sub( uint16_t, uint16_t );
uint16_t hs_f16_mul( uint16_t, uint16_t );
uint16_t hs_f16_mulAdd( uint16_t, uint16_t, uint16_t );
uint16_t hs_f16_div( uint16_t, uint16_t );
uint16_t hs_f16_rem( uint16_t, uint16_t );
uint16_t hs_f16_sqrt( uint16_t );
bool hs_f16_eq( uint16_t, uint16_t );
bool hs_f16_le( uint16_t, uint16_t );
bool hs_f16_lt( uint16_t, uint16_t );
bool hs_f16_eq_signaling( uint16_t, uint16_t );
bool hs_f16_le_quiet( uint16_t, uint16_t );
bool hs_f16_lt_quiet( uint16_t, uint16_t );
bool hs_f16_isSignalingNaN( uint16_t );

uint32_t hs_f32_to_ui32( uint32_t, uint8_t, bool );
uint64_t hs_f32_to_ui64( uint32_t, uint8_t, bool );
int32_t hs_f32_to_i32( uint32_t, uint8_t, bool );
int64_t hs_f32_to_i64( uint32_t, uint8_t, bool );
uint32_t hs_f32_to_f16( uint32_t );
uint64_t hs_f32_to_f64( uint32_t );

uint32_t hs_f32_roundToInt( uint32_t, uint8_t, bool );
uint32_t hs_f32_add( uint32_t, uint32_t );
uint32_t hs_f32_sub( uint32_t, uint32_t );
uint32_t hs_f32_mul( uint32_t, uint32_t );
uint32_t hs_f32_mulAdd( uint32_t, uint32_t, uint32_t );
uint32_t hs_f32_div( uint32_t, uint32_t );
uint32_t hs_f32_rem( uint32_t, uint32_t );
uint32_t hs_f32_sqrt( uint32_t );
bool hs_f32_eq( uint32_t, uint32_t );
bool hs_f32_le( uint32_t, uint32_t );
bool hs_f32_lt( uint32_t, uint32_t );
bool hs_f32_eq_signaling( uint32_t, uint32_t );
bool hs_f32_le_quiet( uint32_t, uint32_t );
bool hs_f32_lt_quiet( uint32_t, uint32_t );
bool hs_f32_isSignalingNaN( uint32_t );


uint32_t hs_f64_to_ui32( uint64_t, uint8_t, bool );
uint64_t hs_f64_to_ui64( uint64_t, uint8_t, bool );
int32_t hs_f64_to_i32( uint64_t, uint8_t, bool );
int64_t hs_f64_to_i64( uint64_t, uint8_t, bool );
uint64_t hs_f64_to_f16( uint64_t );
uint32_t hs_f64_to_f32( uint64_t );

uint64_t hs_f64_roundToInt( uint64_t, uint8_t, bool );
uint64_t hs_f64_add( uint64_t, uint64_t );
uint64_t hs_f64_sub( uint64_t, uint64_t );
uint64_t hs_f64_mul( uint64_t, uint64_t );
uint64_t hs_f64_mulAdd( uint64_t, uint64_t, uint64_t );
uint64_t hs_f64_div( uint64_t, uint64_t );
uint64_t hs_f64_rem( uint64_t, uint64_t );
uint64_t hs_f64_sqrt( uint64_t );
bool hs_f64_eq( uint64_t, uint64_t );
bool hs_f64_le( uint64_t, uint64_t );
bool hs_f64_lt( uint64_t, uint64_t );
bool hs_f64_eq_signaling( uint64_t, uint64_t );
bool hs_f64_le_quiet( uint64_t, uint64_t );
bool hs_f64_lt_quiet( uint64_t, uint64_t );
bool hs_f64_isSignalingNaN( uint64_t );
