import munit._

class Test1 extends FunSuite {
  test("t1") {
    assertEquals("I was compiled by dotty :)", Main.msg)
  }
}
