package raytracer

import org.scalatest.{FeatureSpec, GivenWhenThen, OptionValues}

trait BaseSpec extends FeatureSpec
  with GivenWhenThen with TestHelpers with OptionValues

