// import ps.tricerato.pureimage._
import javax.imageio.ImageIO
import java.io._
import scala.io.Source
import javax.imageio.ImageIO
import java.awt.image.BufferedImage
import javax.imageio.ImageTypeSpecifier
import java.nio.Buffer

object UseFilters {

  type Transform = (BufferedImage, Int, Int) => Int
  type Test = (Int, Int, Int) => Boolean

  def copyEmptyImg(img: BufferedImage): BufferedImage = {
    new BufferedImage(img.getWidth, img.getHeight, img.getType)
  }

  def copyImg(img: BufferedImage): BufferedImage = {
    val out = copyEmptyImg(img)
    for (
      x <- 0 until img.getWidth;
      y <- 0 until img.getHeight
    ) {
      out.setRGB(x, y, img.getRGB(x, y))
    }
    out
  }

  def monochromeImage(width: Int, height: Int, color: Int): BufferedImage = {
    val out = new BufferedImage(width, height, BufferedImage.TYPE_INT_RGB)
    for (
      x <- 0 until width;
      y <- 0 until height
    ) {
      out.setRGB(x, y, color)
    }
    out
  }

  def avgAllPixels(img: BufferedImage): Int = {
    var sum: Int = 0
    for (
      x <- 0 until img.getWidth;
      y <- 0 until img.getHeight
    ) {
      sum += img.getRGB(x, y)
    }
    sum / img.getWidth * img.getHeight
  }

  def avgAllPixels(inPath: String, outPath: String): Unit = {
    val img = ImageIO.read(new File(inPath))
    ImageIO.write(
      monochromeImage(img.getWidth, img.getHeight, avgAllPixels(img)),
      "png",
      new File(outPath)
    )
  }

  def mapImage(img: BufferedImage, transform: Transform): BufferedImage = {
    var out = copyEmptyImg(img)
    for (
      x <- 0 until img.getWidth;
      y <- 0 until img.getHeight
    ) {
      out.setRGB(x, y, transform(img, x, y))
    }
    out
  }

  def mapImage(
      inPath: String,
      outPath: String,
      transform: Transform
  ) {
    ImageIO.write(
      mapImage(ImageIO.read(new File(inPath)), transform),
      "png",
      new File(outPath)
    )
  }

  def filter(img: BufferedImage, test: Test): BufferedImage = {
    mapImage(
      img,
      (img, x, y) => {
        val pixel = img.getRGB(x, y)
        if (test(pixel, x, y)) pixel else 0
      }
    )
  }

  private def getNthPixel(img: BufferedImage, n: Int): Int = {
    val (x, y) = nthPixelCoordinates(img.getWidth, img.getHeight, n)
    img.getRGB(x, y)
  }

  private def setNthPixel(img: BufferedImage, n: Int, rgb: Int): Unit = {
    val (x, y) = nthPixelCoordinates(img.getWidth, img.getHeight, n)
    img.setRGB(x, y, rgb)
  }

  private def nthPixelCoordinates(
      width: Int,
      height: Int,
      n: Int
  ): (Int, Int) = {
    val x = n % width
    val y = (n - x) / height
    (x, y)
  }

  def filter(inPath: String, outPath: String, test: Test) {
    ImageIO.write(
      filter(ImageIO.read(new File(inPath)), test),
      "png",
      new File(outPath)
    )
  }

  // TODO custom transforms
  def fibMap(inPath: String, outPath: String) = {
    val in = ImageIO.read(new File(inPath))
    val out = copyImg(in)
    var i = 1
    var step = 1
    while (i < in.getWidth * in.getHeight) {
      // val pixel = getNthPixel(in, i)
      setNthPixel(out, i, 0)
      step = i
      i += step
    }
    ImageIO.write(
      out,
      "png",
      new File(outPath)
    )
  }

  def correctDimension(bounds: Int, n: Int): Int = {
    (n + bounds) % bounds
  }

  def accessPixelInBounds(img: BufferedImage, x: Int, y: Int): Int = {
    img.getRGB(
      correctDimension(img.getWidth, x),
      correctDimension(img.getHeight, y)
    )
  }

  def avgNeighbors(img: BufferedImage, x: Int, y: Int, distance: Int) = {
    var sum = 0
    var total = 0
    for (
      i <- (x - distance) to (x + distance);
      j <- (y - distance) to (y + distance)
    ) {
      if (
        (i >= 0) && (j >= 0) &&
        (i < img.getWidth) && (j < img.getHeight) &&
        (i != x) && (j != y)
      ) {
        sum += accessPixelInBounds(img, i, j)
        total += 1
      }
    }
    sum / total
  }

  def avgMap(inPath: String, outPath: String, distance: Int) = {
    val img = ImageIO.read(new File(inPath))
    ImageIO.write(
      mapImage(
        img,
        (img, x, y) => {
          avgNeighbors(img, x, y, distance)
        }
      ),
      "png",
      new File(outPath)
    )
  }

  def avgMapNthGeneration(
      inPath: String,
      outPathPrefix: String,
      distance: Int,
      generation: Int
  ): Unit = {
    var img: BufferedImage = ImageIO.read(new File(inPath))
    for (i <- 0 to generation) {
      img = mapImage(
        img,
        (img: BufferedImage, x: Int, y: Int) => {
          avgNeighbors(img, x, y, distance)
        }
      )
    }
    ImageIO.write(
      img,
      "png",
      new File(s"$outPathPrefix-d$distance-${generation}th-generation.png")
    )
  }

  def avgMapIterative(
      inPath: String,
      outPathPrefix: String,
      distance: Int,
      iterations: Int
  ): Unit = {
    var img: BufferedImage = ImageIO.read(new File(inPath))
    for (i <- 0 to iterations) {
      img = mapImage(
        img,
        (img: BufferedImage, x: Int, y: Int) => {
          avgNeighbors(img, x, y, distance)
        }
      )
      ImageIO.write(
        img,
        "png",
        new File(s"$outPathPrefix-d$distance-$i.png")
      )
    }
  }

  def main = {}
}
