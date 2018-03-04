var {Given, Then, When} = require('cucumber')
import Promise from "bluebird"
import 'isomorphic-fetch'
import plot_point_gql from "../support/plot_point_gql"
import {
  expect_ammunition_lists_to_be_the_same,
  expect_arcane_background_lists_to_be_the_same,
  expect_armor_lists_to_be_the_same,
  expect_beast_and_character_common_fields_to_be_the_same,
  expect_edge_lists_to_be_the_same,
  expect_hand_weapon_lists_to_be_the_same,
  expect_hindrance_lists_to_be_the_same,
  expect_mundane_item_lists_to_be_the_same,
  expect_power_lists_to_be_the_same,
  expect_powers_to_be_the_same,
  expect_skill_lists_to_be_the_same
} from "../support/utils"

Given('a plot point exists in the database', function () {
  let db = this.swga_db
  return Promise.all(this.expected.plotPoints.map(plot_point =>
                                                    db.none("insert into plot_points(id, name, description, brief_description) values ($1, $2, $3, $4)", [plot_point.id, plot_point.name, plot_point.description, plot_point.brief_description])
                                                    .then(() => Promise.all(plot_point.ammunition.map(ammo =>
                                                                                                        db.none("insert into ammunition(id, name, description, weight, cost, plot_point_id) values ($1, $2, $3, $4, $5, $6)", [ammo.id, ammo.name, ammo.description, ammo.weight, ammo.cost, plot_point.id])
                                                                                                        .then(() => Promise.all(ammo.notes.map(note => db.none("insert into gear_notes(id, name, description) values ($1, $2, $3)", [note.id, note.name, note.description])
                                                                                                        .then(() => db.none("insert into ammunition_gear_note(ammunition_id, gear_note_id) values ($1, $2)", [ammo.id, note.id]))
                                                                                                        )))
                                                                                                        .then(() => db.none("insert into gear_category(id, name, description, plot_point_id) values($1, $2, $3, $4)", [ammo.category.id, ammo.category.name, ammo.category.description, plot_point.id])
                                                                                                        .then(() => db.none("update ammunition set gear_category_id = $1 where ammunition.id = $2", [ammo.category.id, ammo.id]))
                                                                                                        )
                                                    )))
                                                    .then(() => Promise.all(plot_point.armors.map(armor =>
                                                                                                    db.none("insert into armors(id, name, description, weight, cost, armor, plot_point_id) values ($1, $2, $3, $4, $5, $6, $7)", [armor.id, armor.name, armor.description, armor.weight, armor.cost, armor.armor, plot_point.id])
                                                                                                    .then(() => Promise.all(armor.notes.map(note => db.none("insert into gear_notes(id, name, description) values ($1, $2, $3)", [note.id, note.name, note.description])
                                                                                                    .then(() => db.none("insert into armor_gear_note(armor_id, gear_notes_id) values ($1, $2)", [armor.id, note.id]))
                                                                                                    )))
                                                                                                    .then(() => db.none("insert into gear_category(id, name, description, plot_point_id) values($1, $2, $3, $4)", [armor.category.id, armor.category.name, armor.category.description, plot_point.id])
                                                                                                    .then(() => db.none("update armors set gear_category_id = $1 where armors.id = $2", [armor.category.id, armor.id]))
                                                                                                    )
                                                                                                    .then(() => db.none("insert into technology_levels(id, name, description, plot_point_id) values($1, $2, $3, $4)", [armor.technology_level.id, armor.technology_level.name, armor.technology_level.description, plot_point.id])
                                                                                                    .then(() => db.none("update armors set technology_level_id = $1 where armors.id = $2", [armor.technology_level.id, armor.id]))
                                                                                                    )
                                                    )))
                                                    .then(() => Promise.all(plot_point.skills.map(skill =>
                                                                                                    db.none("insert into skills(id, name, description, plot_point_id, ability) values ($1, $2, $3, $4, $5)", [skill.id, skill.name, skill.description, plot_point.id, skill.ability])
                                                    )))
                                                    .then(() => Promise.all(plot_point.arcaneBackgrounds.map(arcane_background =>
                                                                                                               db.none("insert into arcane_backgrounds( id, name, description, skill_id, starting_powers, starting_power_points, plot_point_id) values( $1, $2, $3, $4, $5, $6, $7)",
                                                                                                                       [arcane_background.id, arcane_background.name, arcane_background.description, arcane_background.skill.id, arcane_background.starting_powers, arcane_background.starting_power_points, plot_point.id])
                                                    )))
                                                    .then(() => Promise.all(plot_point.monstrous_abilities.map(monstrous_ability =>
                                                                                                                 db.none("insert into monstrous_abilities(id, name, description, plot_point_id) values ($1, $2, $3, $4)", [monstrous_ability.id, monstrous_ability.name, monstrous_ability.description, plot_point.id])
                                                    )))
                                                    .then(() => Promise.all(plot_point.beasts.map(beast =>
                                                                                                    db.none(`insert into beasts(id, name, description, plot_point_id, agility, smarts, spirit, strength, vigor, animal_intelligence, pace, parry, toughness, armor, wild_card)
												values ($1, $2, $3, $4, $5, $6, $7, $8, $9, $10, $11, $12, $13, $14, $15)`,
                                                                                                            [beast.id, beast.name, beast.description, plot_point.id, beast.agility, beast.smarts, beast.spirit, beast.strength, beast.vigor, beast.animal_intelligence, beast.pace, beast.parry, beast.toughness, beast.armor, beast.wild_card])
                                                                                                    .then(() => Promise.all(beast.skills.map(skill => db.none("insert into beast_skill(beast_id, skill_id, value, bonus) values( $1, $2, $3, $4)", [beast.id, skill.id, skill.value, skill.bonus]))))
                                                                                                    .then(() => Promise.all(beast.special_abilities.map(special_ability => db.none("insert into beast_monstrous_ability(beast_id, monstrous_ability_id) values( $1, $2)", [beast.id, special_ability.id]))))
                                                    )))
                                                    .then(() => Promise.all(plot_point.edges.map(edge =>
                                                                                                   db.none("insert into edges(id, name, description, effects, plot_point_id) values ($1, $2, $3, $4, $5)", [edge.id, edge.name, edge.description, edge.effects, plot_point.id])
                                                                                                   .then(() => Promise.all(edge.requirements.map(requirement => db.none("insert into edge_requirements(id, requirement, plot_point_id) values ($1, $2, $3)", [requirement.id, requirement.requirement, plot_point.id])
                                                                                                   .then(() => db.none("insert into edge_edge_requirement(edge_id, edge_requirement_id) values ($1, $2)", [edge.id, requirement.id]))
                                                                                                   )))
                                                                                                   .then(() => db.none("insert into edge_types(id, name, description, plot_point_id) values($1, $2, $3, $4)", [edge.type.id, edge.type.name, edge.type.description, plot_point.id])
                                                                                                   .then(() => db.none("update edges set edge_type_id = $1 where edges.id = $2", [edge.type.id, edge.id]))
                                                                                                   )
                                                    )))
                                                    .then(() => Promise.all(plot_point.hand_weapons.map(hand_weapon =>
                                                                                                          db.none("insert into hand_weapons(id, cost, description, name, weight, damage, plot_point_id) values ($1, $2, $3, $4, $5, $6, $7)", [hand_weapon.id, hand_weapon.cost, hand_weapon.description, hand_weapon.name, hand_weapon.weight, hand_weapon.damage, plot_point.id])
                                                                                                          .then(() => Promise.all(hand_weapon.notes.map(note => db.none("insert into gear_notes(id, name, description) values ($1, $2, $3)", [note.id, note.name, note.description])
                                                                                                          .then(() => db.none("insert into hand_weapon_gear_note(hand_weapon_id, gear_notes_id) values ($1, $2)", [hand_weapon.id, note.id]))
                                                                                                          )))
                                                                                                          .then(() => db.none("insert into gear_category(id, name, description, plot_point_id) values($1, $2, $3, $4)", [hand_weapon.category.id, hand_weapon.category.name, hand_weapon.category.description, plot_point.id])
                                                                                                          .then(() => db.none("update hand_weapons set gear_category_id = $1 where hand_weapons.id = $2", [hand_weapon.category.id, hand_weapon.id]))
                                                                                                          )
                                                                                                          .then(() => db.none("insert into technology_levels(id, name, description, plot_point_id) values($1, $2, $3, $4)", [hand_weapon.technology_level.id, hand_weapon.technology_level.name, hand_weapon.technology_level.description, plot_point.id])
                                                                                                          .then(() => db.none("update hand_weapons set technology_level_id = $1 where hand_weapons.id = $2", [hand_weapon.technology_level.id, hand_weapon.id]))
                                                                                                          )
                                                    )))
                                                    .then(() => Promise.all(plot_point.hindrances.map(hindrance =>
                                                                                                        db.none("insert into hindrances(id, description, name, type, plot_point_id) values ($1, $2, $3, $4, $5)", [hindrance.id, hindrance.description, hindrance.name, hindrance.type, plot_point.id])
                                                    )))
                                                    .then(() => Promise.all(plot_point.mundane_items.map(mundane_item =>
                                                                                                           db.none("insert into mundane_items(id, cost, description, name, weight, plot_point_id) values ($1, $2, $3, $4, $5, $6)", [mundane_item.id, mundane_item.cost, mundane_item.description, mundane_item.name, mundane_item.weight, plot_point.id])
                                                                                                           .then(() => Promise.all(mundane_item.notes.map(note => db.none("insert into gear_notes(id, name, description) values ($1, $2, $3)", [note.id, note.name, note.description])
                                                                                                           .then(() => db.none("insert into mundane_item_gear_note(mundane_item_id, gear_notes_id) values ($1, $2)", [mundane_item.id, note.id]))
                                                                                                           )))
                                                                                                           .then(() => db.none("insert into gear_category(id, name, description, plot_point_id) values($1, $2, $3, $4)", [mundane_item.category.id, mundane_item.category.name, mundane_item.category.description, plot_point.id])
                                                                                                           .then(() => db.none("update mundane_items set gear_category_id = $1 where mundane_items.id = $2", [mundane_item.category.id, mundane_item.id]))
                                                                                                           )
                                                                                                           .then(() => db.none("insert into technology_levels(id, name, description, plot_point_id) values($1, $2, $3, $4)", [mundane_item.technology_level.id, mundane_item.technology_level.name, mundane_item.technology_level.description, plot_point.id])
                                                                                                           .then(() => db.none("update mundane_items set technology_level_id = $1 where mundane_items.id = $2", [mundane_item.technology_level.id, mundane_item.id]))
                                                                                                           )
                                                    )))
                                                    .then(() => Promise.all(plot_point.powers.map(power => db.none("insert into powers (id, plot_point_id, name, description, rank, power_points, range, duration, trappings) values($1, $2, $3, $4, $5, $6, $7, $8, $9)",
                                                                                                                   [power.id, plot_point.id, power.name, power.description, power.rank, power.power_points, power.range, power.duration, power.trappings])
                                                    .then(() => db.none("insert into power_arcane_backgrounds(power_id, arcane_background_id) values( $1, $2)", [power.id, power.available_to.id])))))
                                                    .then(() => Promise.all(plot_point.trappings.map(trapping =>
                                                                                                       db.none("insert into trappings (id, plot_point_id, name, description) values ($1, $2, $3, $4)", [trapping.id, plot_point.id, trapping.name, trapping.description])
                                                                                                       .then(() => Promise.all(trapping.effects.map(effect => db.none("insert into trapping_effects (id, trapping_id, name, description) values($1, $2, $3, $4)", [effect.id, trapping.id, effect.name, effect.description])))))))
                                                    .then(() => Promise.all(plot_point.characters.map(character =>
                                                                                                        db.none("insert into characters (id, agility, armor, charisma, description, experience_points, name, pace, parry, power_points, race_id, rank, smarts, spirit, strength, toughness, vigor, wild_card, plot_point_id)" +
                                                                                                          "values($1, $2, $3, $4, $5, $6, $7, $8, $9, $10, $11, $12, $13, $14, $15, $16, $17, $18, $19)",
                                                                                                          [character.id, character.agility, character.armor, character.charisma, character.description, character.experience_points, character.name, character.pace, character.parry, character.power_points, character.race_id, character.rank, character.smarts, character.spirit, character.strength, character.toughness, character.vigor, character.wild_card, plot_point.id])
                                                                                                        .then(() => Promise.all(character.ammunition.map(ammo =>
                                                                                                                                                           db.none("insert into character_ammunition(character_id, ammunition_id, amount) values($1, $2, $3)", [character.id, ammo.id, ammo.amount]))))
                                                                                                        .then(() => Promise.all(character.edges.map(edge =>
                                                                                                                                                      db.none('insert into character_edge(character_id, edge_id) values ($1, $2)', [character.id, edge.id]))))
                                                                                                        .then(() => Promise.all(character.hand_weapons.map(hand_weapon =>
                                                                                                                                                             db.none('insert into character_hand_weapon(character_id, hand_weapon_id, amount) values ($1, $2, $3)', [character.id, hand_weapon.id, hand_weapon.amount]))))
                                                                                                        .then(() => Promise.all(character.hindrances.map(hindrance =>
                                                                                                                                                           db.none('insert into character_hindrance(character_id, hindrance_id, taken_as) values ($1, $2, $3)', [character.id, hindrance.id, hindrance.taken_as]))))
                                                                                                        .then(() => Promise.all(character.mundane_items.map(mundane_item =>
                                                                                                                                                              db.none('insert into character_mundane_item(character_id, mundane_item_id, amount) values ($1, $2, $3)', [character.id, mundane_item.id, mundane_item.amount]))))
                                                                                                        .then(() => Promise.all(character.powers.map(power => db.none('insert into character_power(id, name, character_id, power_id, trapping_id, notes) values ($1, $2, $3, $4, $5, $6)', [power.id, power.name, character.id, power.power.id, power.trapping.id, power.notes]))))
                                                    )))
  ))
})

When('I query for all plot points', function () {
  let client = this.plotPointService
  return client.query({
                        query: plot_point_gql,
                        variables:
                          {
                            pagination: {
                              start: 0,
                              limit: 100
                            }
                          }
                      })
  .then(result => this.result.data = result)
  // .catch(error => this.result.error = error)
})

Then('I get a list of all plot points in the database', function (callback) {
  expect(this.result.data).to.be.ok
  expect(this.result.error).to.not.be.ok
  expect(this.result.data.data.plotPoints).to.be.ok
  const plotPoints = this.result.data.data.plotPoints
  expect(plotPoints).to.not.be.empty
  plotPoints.map((plot_point, index) => {
    let expected_plot_point = this.expected.plotPoints[index]
    expect(plot_point.id).to.be.equal(expected_plot_point.id)
    expect(plot_point.name).to.be.equal(expected_plot_point.name)
    expect(plot_point.description).to.be.equal(expected_plot_point.description)
    expect(plot_point.brief_description).to.be.equal(expected_plot_point.brief_description)
    expect_ammunition_lists_to_be_the_same(plot_point.ammunition, expected_plot_point.ammunition)
    expect_arcane_background_lists_to_be_the_same(plot_point.arcaneBackgrounds, expected_plot_point.arcaneBackgrounds)
    expect_armor_lists_to_be_the_same(plot_point.armors, expected_plot_point.armors)
    expect(plot_point.beasts.length).to.be.equal(expected_plot_point.beasts.length)
    plot_point.beasts.forEach((beast, beast_index) => {
      let expected_beast = expected_plot_point.beasts[beast_index]
      expect_beast_and_character_common_fields_to_be_the_same(beast, expected_beast)
      expect_skill_lists_to_be_the_same(beast.skills, expected_beast.skills)
      expect(beast.special_abilities.length).to.be.above(0)
      beast.special_abilities.map((special_ability, special_ability_index) => {
        let expected_special_ability = expected_beast.special_abilities[special_ability_index]
        expect(special_ability.id).to.be.equal(expected_special_ability.id)
        expect(special_ability.name).to.be.equal(expected_special_ability.name)
        expect(special_ability.description).to.be.equal(expected_special_ability.description)
      })
    })
    expect(plot_point.characters.length).to.be.equal(expected_plot_point.characters.length)
    plot_point.characters.forEach((character, character_index) => {
      let expected_character = expected_plot_point.characters[character_index]
      expect_beast_and_character_common_fields_to_be_the_same(character, expected_character)
      expect_ammunition_lists_to_be_the_same(character.ammunition, expected_character.ammunition)
      expect_skill_lists_to_be_the_same(character.skills, expected_character.skills)
      expect_edge_lists_to_be_the_same(character.edges, expected_character.edges)
      expect_hand_weapon_lists_to_be_the_same(character.hand_weapons, expected_character.hand_weapons)
      expect_hindrance_lists_to_be_the_same(character.hindrances, expected_character.hindrances)
      expect_mundane_item_lists_to_be_the_same(character.mundane_items, expected_character.mundane_items)
      character.powers.forEach((power, index) => {
        expect(power.id).to.be.equal(expected_character.powers[index].id)
        expect(power.name).to.be.equal(expected_character.powers[index].name)
        expect(power.notes).to.be.equal(expected_character.powers[index].notes)
        expect_powers_to_be_the_same(power.power, expected_character.powers[index].power)
      })
      expect(character.experience_points).to.be.equal(expected_character.experience_points)
      expect(character.power_points).to.be.equal(expected_character.power_points)
      expect(character.rank).to.be.equal(expected_character.rank)
    })
    expect_edge_lists_to_be_the_same(plot_point.edges, expected_plot_point.edges)
    expect_hand_weapon_lists_to_be_the_same(plot_point.hand_weapons, expected_plot_point.hand_weapons)
    expect_hindrance_lists_to_be_the_same(plot_point.hindrances, expected_plot_point.hindrances)
    expect_mundane_item_lists_to_be_the_same(plot_point.mundane_items, expected_plot_point.mundane_items)
    expect_power_lists_to_be_the_same(plot_point.powers, expected_plot_point.powers)
    expect_skill_lists_to_be_the_same(plot_point.skills, expected_plot_point.skills)

  })
  callback()
})