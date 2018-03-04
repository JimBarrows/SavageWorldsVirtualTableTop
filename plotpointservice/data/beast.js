import database from '../database'

export function beasts (plotpointId) {
  return database.query("select id, name, description, plot_point_id, agility, smarts, spirit, strength, vigor, animal_intelligence, pace, parry, toughness, armor, wild_card from beasts where plot_point_id = $1 order by name", plotpointId)
}

export function beast_skills (beastId) {
  return database.query(`select skills.id as id,
        skills.name as name,
        skills.description as description,
        skills.ability as ability,
        beast_skill.value as value,
        beast_skill.bonus as bonus
      from beasts, beast_skill, skills
      where beast_skill.beast_id = $1
        and beast_skill.skill_id = skills.id
      order by name`, beastId);
}

export function monstrous_abilities (beastId) {
  return database.query(`
      select monstrous_abilities.id as id,
      monstrous_abilities.name as name,
      monstrous_abilities.description as description
      from beasts, monstrous_abilities, beast_monstrous_ability
      where beast_monstrous_ability.beast_id = $1
        and beast_monstrous_ability.monstrous_ability_id = monstrous_abilities.id
    `, beastId);
}
