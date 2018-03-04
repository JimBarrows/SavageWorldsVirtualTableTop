export const EdgeRequirement = `
type EdgeRequirement {
  id: ID!,
  requirement: String!
}
`

export const EdgeType = `
type EdgeType {
  id: ID!,
  name: String!,
  description: String!
}
`

export const Edge = `
type Edge {
  id: ID!,
  name: String!,
  description: String!,
  effects: String!,
  requirements: [EdgeRequirement!]!,
  type: EdgeType!
}`

