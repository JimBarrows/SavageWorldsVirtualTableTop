export default `
  type MundaneItem implements GearInterface {
    id: ID!
    category: GearCategory!
    cost: Int!
    description: String!
    name: String!
    notes: [GearNote]!
    technology_level: TechnologyLevel!
    weight: Int
  }
`
