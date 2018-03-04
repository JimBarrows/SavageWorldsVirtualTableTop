export default `
type Power {
  id: ID!
  name: String!
  description: String!
  rank: Rank!,
  power_points: Int!,
  range: String!,
  duration: String!,
  trappings: String!,
  available_to: [ArcaneBackground]
}
`
