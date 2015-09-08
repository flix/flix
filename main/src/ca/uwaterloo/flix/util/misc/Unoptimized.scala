package ca.uwaterloo.flix.util.misc

import scala.annotation.StaticAnnotation

/**
 * An annotation placed to indicate potentially slow code.
 */
class Unoptimized extends StaticAnnotation
