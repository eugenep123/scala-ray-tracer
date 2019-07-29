package raytracer

import org.scalatest.{FeatureSpec, GivenWhenThen}
import raytracer.files.PpmWriter
import raytracer.patterns.{CheckersPattern, GradientPattern, RingPattern, StripePattern}
import raytracer.shapes.Sphere

trait BaseSpec extends FeatureSpec
  with GivenWhenThen with TestHelpers

