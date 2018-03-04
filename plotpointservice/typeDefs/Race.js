export const RaceAbility = `
type RaceAbility {
  id: ID!,
  name: String!,
  description: String!,
  cost: Int!
}`

export const Race = `
type Race {
  id: ID!,
  name: String!,
  description: String!,
  abilities: [RaceAbility]
}
`

