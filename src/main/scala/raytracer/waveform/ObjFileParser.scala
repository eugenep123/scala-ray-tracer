package raytracer.waveform

import raytracer.shapes.Group

object ObjFileParser {

  def parseContent(content: String): ParserResult = {
    import ObjGrammar._
    import fastparse._
    val builder = new ParserResult()
    val parsed = parse(content, builderParser(builder)(_), startIndex = 0)
    parsed.get
    builder
  }

  def parseGroup(content: String): Group = parseContent(content).toGroup

}
