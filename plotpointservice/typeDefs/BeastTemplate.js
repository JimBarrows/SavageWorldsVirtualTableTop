export default `
interface BeastTemplate {
  id: ID!
  name: String!
  agility: AttributeValue!
  smarts: AttributeValue!
  spirit: AttributeValue!
  strength: AttributeValue!
  vigor: AttributeValue!
  skills: [OwnedSkill]!
  pace: Int!
  parry: Int!
  toughness: Int!
  armor: Int!
  wild_card: Boolean
}
`
