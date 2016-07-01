import scalanative.native._, stdlib._, stdio._

@struct
class i1(val _1: Int = 0)
@struct
class i2(val _1: Int = 0, val _2: Int = 0)
@struct
class i3(val _1: Int = 0, val _2: Int = 0, val _3: Int = 0)
@struct
class i4(val _1: Int = 0, val _2: Int = 0, val _3: Int = 0, val _4: Int = 0)
@struct
class i5(val _1: Int = 0, val _2: Int = 0, val _3: Int = 0, val _4: Int = 0, val _5: Int = 0)
@struct
class i6(val _1: Int = 0,
         val _2: Int = 0,
         val _3: Int = 0,
         val _4: Int = 0,
         val _5: Int = 0,
         val _6: Int = 0)
@struct
class i7(val _1: Int = 0,
         val _2: Int = 0,
         val _3: Int = 0,
         val _4: Int = 0,
         val _5: Int = 0,
         val _6: Int = 0,
         val _7: Int = 0)
@struct
class i8(val _1: Int = 0,
         val _2: Int = 0,
         val _3: Int = 0,
         val _4: Int = 0,
         val _5: Int = 0,
         val _6: Int = 0,
         val _7: Int = 0,
         val _8: Int = 0)

@struct
class Vec(val x: Double, val y: Double, val z: Double)

@struct
class Ray(val o: Vec, val d: Vec)

@extern
object dummy {

  def takei1(i: i1): Unit = extern
  def takei2(i: i2): Unit = extern
  def takei3(i: i3): Unit = extern
  def takei4(i: i4): Unit = extern
  def takei5(i: i5): Unit = extern
  def takei6(i: i6): Unit = extern
  def takei7(i: i7): Unit = extern
  def takei8(i: i8): Unit = extern

  def makei3(i: Int): i3 = extern
  def makei8(i: Int): i8 = extern

  def takeRay(r: Ray): Unit = extern

}

object Test {
  import dummy._
  def main(args: Array[String]): Unit = {
//    takei1(new i1())
//    takei2(new i2())
//    takei3(new i3())
//    takei4(new i4())
//    takei5(new i5())
//    takei6(new i6())
//    takei7(new i7())
//    takei8(new i8())
//    val _i3 = makei3(10)
//    takei3(_i3)
//    val _i8 = makei8(20)
//    takei8(_i8)
//    test(new i3(10, 20, 30))
//    test(new i8(10, 20, 30, 40, 50, 60, 70, 80))
    testRay(new Ray(new Vec(1, 2, 3), new Vec(4, 5, 6)))
  }

  def test(foo: i3): Unit = {
    fprintf(stdout, c"i3: %d %d %d\n", foo._1, foo._2, foo._3)
  }

  def test(foo: i8): Unit = {
    fprintf(stdout, c"i8: %d %d %d %d %d %d %d %d\n", foo._1, foo._2, foo._3, foo._4, foo._5, foo._6, foo._7, foo._8)
  }

  def testRay(ray: Ray): Unit = {
    fprintf(stdout, c"ray: %g %g %g -> %g %g %g\n", ray.o.x, ray.o.y, ray.o.z, ray.d.x, ray.d.y, ray.d.z)
  }

}
