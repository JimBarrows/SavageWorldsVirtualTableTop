export default `
type RangedWeapon implements GearInterface {
  id: ID!
  category: GearCategory!
  cost: Int!
  description: String!
  name: String!
  notes: [GearNote]!
  technology_level: TechnologyLevel!
  weight: Int
  damage: String!
  short: Int!
  medium: Int!
  long: Int!
  rate_of_fire: Int!
  shots: Int
  minimum_strength: String
}
`
