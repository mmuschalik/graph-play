package Model

case class Resource(name: String, identity: Attribute, attributes: Attribute*)
case class Attribute(name: String)

enum Relationship {
  case ManyToOne(refAttribute: Attribute)
  case OneToMany(refAttribute: Attribute)
  case OneToOne(refAttribute: Attribute)
}