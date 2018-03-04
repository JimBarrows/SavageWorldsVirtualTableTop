export default `
type AmmunitionAmount {
  id: ID!
  name: String!
  description: String!
  weight: Int!
  cost: Int!
  notes: [GearNote]!
  category: GearCategory
  amount: Int!
}
`