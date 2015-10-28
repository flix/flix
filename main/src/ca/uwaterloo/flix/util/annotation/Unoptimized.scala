package ca.uwaterloo.flix.util.annotation

import scala.annotation.StaticAnnotation

/**
 * An annotation placed to indicate potentially slow code.
 */
class Unoptimized extends StaticAnnotation
