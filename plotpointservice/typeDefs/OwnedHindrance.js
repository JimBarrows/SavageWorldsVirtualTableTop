export default `
type OwnedHindrance {
  id: ID!,
  name: String!,
  description: String!,
  type: HindranceType!,
  taken_as: HindranceType!
}
`
