export default `
type PlotPoint {
  id: ID!
  name: String!
  description: String
  brief_description: String
  ammunition(pagination: Pagination!): [Ammunition]!
  arcaneBackgrounds(pagination: Pagination!): [ArcaneBackground]!
  armors(pagination: Pagination!): [Armor]!
  beasts(pagination: Pagination!): [Beast]!
  characters(pagination: Pagination!): [Character]!
  edges( pagination: Pagination!): [Edge]!
  hand_weapons( pagination: Pagination!): [HandWeapon]!
  hindrances( pagination: Pagination!): [Hindrance]!
  mundane_items( pagination: Pagination!): [MundaneItem]!
  powers( pagination: Pagination!): [Power]!
  skills( pagination: Pagination!): [Skill]!
}
`