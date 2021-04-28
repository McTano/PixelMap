import UseFilters._
class UseFiltersTest extends org.scalatest.funsuite.AnyFunSuite {

  case class TestImage(name: String, path: String) {}

  val testImgs = List(
    TestImage("zardoz", "images/zardoz.png"),
    TestImage("vancouver", "images/vancouver.png"),
    TestImage("cohen", "images/cohen.jpg")
  )

  def outputPath(testName: String, img: TestImage): String = {
    s"output/${img.name}/${testName}.png"
  }

  test("hello world") {
    assert(true)
  }
  test("sanity test for identity function") {
    for (img <- testImgs) {
      UseFilters.filter(
        img.path,
        outputPath("id-filter", img),
        (_, _, _) => true
      )
      UseFilters.mapImage(
        img.path,
        outputPath("id-map", img),
        (img, x, y) => img.getRGB(x, y)
      )
    }
  }

  test("filter out odd pixels") {
    for (img <- testImgs) {
      UseFilters.filter(
        img.path,
        outputPath("even", img),
        (_, x, y) => { (x + y) % 2 == 0 }
      )
    }

    assert(true)
  }

  test("every nth pixel") {
    for (
      img <- testImgs;
      n <- List(2, 5, 7, 13)
    ) {
      UseFilters.filter(
        img.path,
        outputPath(s"every-${n}th-pixel", img),
        (_, x, y) => { (x + y) % n == 0 }
      )
    }
    assert(true)
  }

  test("map pixels to position as rgb") {
    for (img <- testImgs) {
      UseFilters.mapImage(
        img.path,
        outputPath("position", img),
        (_, x, y) => { (x + y) % 256 }
      )
    }
  }

  test("multiply position with rgb value") {
    for (img <- testImgs) {
      UseFilters.mapImage(
        img.path,
        outputPath("position-and-rgb", img),
        (img, x, y) => (x + (y * img.getRGB(x, y))) % 256
      )
    }
  }

  test("blot out fibonacci pixels") {
    for (img <- testImgs) {
      UseFilters.fibMap(
        img.path,
        outputPath("fib-black", img)
      )
    }
  }

  test(
    s"vary average of surrounding pixels with increasing distance"
  ) {
    for (
      img <- testImgs;
      distance <- 1 to 1000
    ) {
      UseFilters.avgMap(
        img.path,
        outputPath(s"avgNeighbours-packed${distance}", img),
        distance
      )
      UseFilters3Byte.avgMap(
        img.path,
        outputPath(s"avgNeighbours-3byte-${distance}", img),
        distance
      )
    }
  }

  test("average of all pixels") {
    for (img <- testImgs) {
      UseFilters.avgAllPixels(img.path, outputPath("avgAllPixels-packed", img))
      UseFilters3Byte.avgAllPixels(
        img.path,
        outputPath("avgAllPixels-3byte", img)
      )
    }
  }

  test("avg neighbour pixel applied iteratively") {
    for (img <- testImgs) {
      UseFilters
        .avgMapIterative(
          img.path,
          s"output/${img.name}/avgNeighbours-iterative",
          1,
          1000
        )
    }
  }

  test(
    "avg neighbour pixel applied iteratively w/o writing each change to file system"
  ) {
    for (img <- testImgs) {
      UseFilters
        .avgMapNthGeneration(
          img.path,
          s"output/${img.name}/avgNeighbours-iterative",
          1,
          200
        )
    }
  }
}
