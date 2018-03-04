export default `
type Vehicle {
  id: ID!
  name: String!
  description: String!
  acceleration: Int!
  top_speed: Int!
  overall_toughness: Int
  overall_armor: Int
  front_toughness: Int
  front_armor: Int
  side_toughness: Int
  side_armor: Int
  back_toughness: Int
  back_armor: Int
  armor: Int!
  crew: Int!
  passengers: Int!
  technology_level: TechnologyLevel!
  min_cost: Int!
  max_cost: Int!
  notes: [GearNote]!
  weapons: [VehicleMountedAndAtGun]
  type: [VehicleType]!
}
`

