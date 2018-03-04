export default `
  type VehicleMountedAndAtGun {
    id: ID!
    name: String!
    description: String!
    short: Int!
    medium: Int!
    long: Int!
    armor_piercing_round: VehicleMountedAndAtGunRound
    high_explosive_round: VehicleMountedAndAtGunRound
    rate_of_fire: Int!
  }
`