/**
 * An implementation of the ``type'' abstract domain.
 *
 * See Cousot & Cousot '92 for details.
 */
namespace Type {

    enum Type {
                   case Err,

        case Bool, case Int, case Real,

                   case Bot
    }

}
// TODO
//integer + integer = integer integer I integer = Boolean
//integer + real = real integer I real = Boolean
//real + integer = real real I integer = Boolean
//real + real = real real I real = Boolean
//
//integer + error =error
//integer + Boolean = error error + integer =error
//Boolean + integer = error real + error =error
//Boolean + Boolean = error error + real = error
//real + Boolean = error Boolean + error = error
//Boolean + real = error error + Boolean = error
//error + error =error