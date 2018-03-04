export default `
type Armor implements GearInterface {
  id: ID!
  category: GearCategory!
  cost: Int!
  description: String!
  name: String!
  notes: [GearNote]!
  technology_level: TechnologyLevel!
  weight: Int!
  armor: Int!
}
`