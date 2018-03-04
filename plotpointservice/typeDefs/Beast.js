export default `
type Beast implements BeastTemplate {
  id: ID!
  name: String!
  agility: AttributeValue!
  smarts: AttributeValue!
  spirit: AttributeValue!
  strength: AttributeValue!
  vigor: AttributeValue!
  animal_intelligence: Boolean!
  skills: [OwnedSkill]!
  pace: Int!
  parry: Int!
  toughness: Int!
  armor: Int!
  special_abilities: [MonstrousAbility]!
  wild_card: Boolean!
  description: String!
}
`