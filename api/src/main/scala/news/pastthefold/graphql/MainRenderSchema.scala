package news.pastthefold.graphql

import ammonite.ops._
import sangria.renderer.SchemaRenderer

object MainRenderSchema extends App {
  val outFile = args(0)
  val schemaStr = SchemaRenderer.renderSchema(SchemaDefinition.schemaDefinition)
  write.over(Path(outFile), schemaStr)
}
