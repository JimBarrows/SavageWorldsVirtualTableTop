export default `
type Character implements BeastTemplate {
  id: ID!
  agility: AttributeValue!
  ammunition: [AmmunitionAmount]!
  armor: Int!
  charisma: Int!
  description: String!
  edges: [Edge]!
  experience_points: Int!
  fatigue: Int!
  hand_weapons: [HandWeapon]!
  hindrances: [OwnedHindrance]!
  mundane_items: [MundaneItem]!
  name: String!
  pace: Int!
  parry: Int!
  power_points: Int!
  powers: [CharacterPower]!
  race: Race!
  ranged_weapons: [RangedWeapon]!
  rank: Rank!
  skills: [OwnedSkill]!
  smarts: AttributeValue!
  special_weapons: [SpecialWeapon]!
  spirit: AttributeValue!
  strength: AttributeValue!
  toughness: Int!
  vehicle_mounted_and_AT_guns: [VehicleMountedAndAtGun]!
  vehicles: [Vehicle]!
  vigor: AttributeValue!
  wild_card: Boolean!,
  wounds: Int!
}
`
