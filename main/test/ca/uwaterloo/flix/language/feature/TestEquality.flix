//
// # Unit
//
@test
def eqUnit: Bool = assert!(() == ())

//
// # Bool
//

@test
def eqBool01: Bool = assert!(true == true)

@test
def eqBool02: Bool = assert!(false == false)

@test
def neqBool01: Bool = assert!(true != false)

@test
def neqBool02: Bool = assert!(false != true)

//
// # Char
//

@test
def eqChar01: Bool = assert!('a' == 'a')

@test
def eqChar02: Bool = assert!('z' == 'z')

@test
def neqChar01: Bool = assert!('a' != 'b')

@test
def neqChar02: Bool = assert!('z' != 'w')

//
// # Float32
//

@test
def eqFloat3201: Bool = assert!(+0.0f32 == +0.0f32)

@test
def eqFloat3202: Bool = assert!(-0.0f32 == -0.0f32)

@test
def eqFloat3203: Bool = assert!(+1.0f32 == +1.0f32)

@test
def eqFloat3204: Bool = assert!(-1.0f32 == -1.0f32)

@test
def neqFloat3201: Bool = assert!(-1.0f32 != +1.0f32)

@test
def neqFloat3202: Bool = assert!(+1.0f32 != -1.0f32)

//
// # Float64
//

@test
def eqFloat6401: Bool = assert!(+0.0f64 == +0.0f64)

@test
def eqFloat6402: Bool = assert!(-0.0f64 == -0.0f64)

@test
def eqFloat6403: Bool = assert!(+1.0f64 == +1.0f64)

@test
def eqFloat6404: Bool = assert!(-1.0f64 == -1.0f64)

@test
def neqFloat6401: Bool = assert!(-1.0f64 != +1.0f64)

@test
def neqFloat6402: Bool = assert!(+1.0f64 != -1.0f64)

