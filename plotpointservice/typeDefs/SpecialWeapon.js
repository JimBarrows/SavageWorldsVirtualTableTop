export default `
type SpecialWeapon implements GearInterface {
  id: ID!
  category: GearCategory!
  cost: Int!
  description: String!
  name: String!
  notes: [GearNote]!
  technology_level: TechnologyLevel!
  weight: Int
  short: Int!
  medium: Int!
  long: Int!
  damage: String!
  rate_of_fire: Int
  armor_pierce: Int
  minimum_strength: String
  burst_template: BurstTemplate

}`