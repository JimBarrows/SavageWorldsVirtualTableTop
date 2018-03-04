export default `
type HandWeapon implements GearInterface {
  id: ID!
  category: GearCategory!
  cost: Int!
  description: String!
  name: String!
  notes: [GearNote]!
  technology_level: TechnologyLevel!
  weight: Int
  damage: String!
}
`
