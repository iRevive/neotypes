package neotypes

import java.util.{Map => JMap}

import mappers.ParameterMapper

import org.neo4j.driver.types.{Path => NPath}

import scala.jdk.CollectionConverters._

object types {
  final case class Path[N, R](nodes: Seq[N], relationships: Seq[R], path: NPath)

  /** Safe wrapper over a Neo4j parameter. */
  final class QueryParam private[neotypes] (val underlying: AnyRef) extends AnyVal

  object QueryParam {
    def apply[A](scalaValue: A)(implicit mapper: ParameterMapper[A]): QueryParam =
      mapper.toQueryParam(scalaValue)

    def toJavaMap(map: Map[String, QueryParam]): JMap[String, Object] =
      map.map {
        case (key, value) => key -> value.underlying
      }.asJava
  }
}
