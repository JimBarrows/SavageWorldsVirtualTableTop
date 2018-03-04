export const HindranceType = `
enum HindranceType {
  minor,
  major
  minor_or_major
}
`

export const Hindrance = `
type Hindrance {
  id: ID!,
  name: String!,
  description: String!,
  type: HindranceType!
}
`
