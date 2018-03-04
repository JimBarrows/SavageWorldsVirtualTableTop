export function expect_ammunition_lists_to_be_the_same (actual, expected) {
  if (expected) {
    expect(actual.length).to.be.equal(expected.length)
    actual.map((actual_item, index) => {
      let expected_item = expected[index]
      expect_gear_common_fields_to_be_the_same(actual_item, expected_item)

    })
  } else {
    expect(actual).to.not.be.ok
  }
}

export function expect_arcane_background_to_be_the_same (actual, expected) {
  expect(actual.id).to.be.equal(expected.id)
  expect(actual.name).to.be.equal(expected.name)
  expect(actual.description).to.be.equal(expected.description)
  expect(actual.starting_powers).to.be.equal(expected.starting_powers)
  expect(actual.starting_power_points).to.be.equal(expected.starting_power_points)
  expect_skill_to_be_the_same(actual.skill, expected.skill)
}

export function expect_arcane_background_lists_to_be_the_same (actual, expected) {
  expect(actual.length).to.be.equal(expected.length)
  actual.map((actual_item, index) => {
    let expected_item = expected[index]
    expect_arcane_background_to_be_the_same(actual_item, expected_item)
  })
}

export function expect_armor_lists_to_be_the_same (actual, expected) {
  expect(actual.length).to.be.equal(expected.length)
  actual.map((actual_item, index) => {
    let expected_item = expected[index]
    expect_gear_common_fields_to_be_the_same(actual_item, expected_item)
    expect(actual_item.armor).to.be.equal(expected_item.armor)
    expect_techology_levels_to_be_the_same(actual_item.category, expected_item.category)
  })
}

export function expect_beast_and_character_common_fields_to_be_the_same (actual, expected) {
  expect(actual.id).to.be.equal(expected.id)
  expect(actual.name).to.be.equal(expected.name)
  expect(actual.description).to.be.equal(expected.description)
  expect(actual.agility).to.be.equal(expected.agility)
  expect(actual.smarts).to.be.equal(expected.smarts)
  expect(actual.spirit).to.be.equal(expected.spirit)
  expect(actual.strength).to.be.equal(expected.strength)
  expect(actual.vigor).to.be.equal(expected.vigor)
  expect(actual.pace).to.be.equal(expected.pace)
  expect(actual.parry).to.be.equal(expected.parry)
  expect(actual.toughness).to.be.equal(expected.toughness)
  expect(actual.armor).to.be.equal(expected.armor)
  expect(actual.wild_card).to.be.equal(expected.wild_card)
}

export function expect_categorys_to_be_the_same (actual, expected) {
  expect(actual.id).to.be.equal(expected.id)
  expect(actual.name).to.be.equal(expected.name)
  expect(actual.description).to.be.equal(expected.description)
}

export function expect_edge_lists_to_be_the_same (actual, expected) {
  expect(actual.length).to.be.equal(expected.length)
  actual.forEach((actual_item, index) => {
    let expected_item = expected[index]
    expect(actual_item.id).to.be.equal(expected_item.id)
    expect(actual_item.name).to.be.equal(expected_item.name)
    expect(actual_item.description).to.be.equal(expected_item.description)
    expect(actual_item.effects).to.be.equal(expected_item.effects)
    expect(actual_item.type.id).to.be.equal(expected_item.type.id)
    expect(actual_item.type.name).to.be.equal(expected_item.type.name)
    expect(actual_item.type.description).to.be.equal(expected_item.type.description)
    expect(actual_item.requirements.forEach((requirement, requirement_index) => {
      let expected_requirement = expected_item.requirements[requirement_index]
      expect(requirement.id).to.be.equal(expected_requirement.id)
      expect(requirement.name).to.be.equal(expected_requirement.name)
      expect(requirement.description).to.be.equal(expected_requirement.description)
    }))
  })
}

export function expect_gear_common_fields_to_be_the_same (actual, expected) {
  expect(actual.id).to.be.equal(expected.id)
  expect(actual.name).to.be.equal(expected.name)
  expect(actual.description).to.be.equal(expected.description)
  expect(actual.weight).to.be.equal(expected.weight)
  expect(actual.cost).to.be.equal(expected.cost)
  expect_categorys_to_be_the_same(actual.category, expected.category)
  expect_note_lists_to_be_the_same(actual.notes, expected.notes)
}

export function expect_hand_weapon_lists_to_be_the_same (actual, expected) {
  expect(actual.length).to.be.equal(expected.length)
  actual.forEach((actual_item, index) => {
    let expected_item = expected[index]
    expect_gear_common_fields_to_be_the_same(actual_item, expected_item)
    expect_techology_levels_to_be_the_same(actual_item.category, expected_item.category)
    expect(actual_item.damage).to.be.equal(expected_item.damage)
  })
}

export function expect_hindrance_lists_to_be_the_same (actual, expected) {
  expect(actual.length).to.be.equal(expected.length)
  actual.map((actual_item, index) => {
    let expected_item = expected[index]
    expect(actual_item.id).to.be.equal(expected_item.id)
    expect(actual_item.name).to.be.equal(expected_item.name)
    expect(actual_item.description).to.be.equal(expected_item.description)
    expect(actual_item.type).to.be.equal(expected_item.type)
    if (expected_item.taken_as) {
      expect(actual_item.taken_as).to.be.equal(expected_item.taken_as)
    } else {
      expect(actual_item.taken_as).to.be.undefined
    }
  })
}

export function expect_mundane_item_lists_to_be_the_same (actual, expected) {

  expect(actual.length).to.be.equal(expected.length)
  actual.map((actual_item, index) => {
    let expected_item = expected[index]
    expect_gear_common_fields_to_be_the_same(actual_item, expected_item)
    expect_techology_levels_to_be_the_same(actual_item.category, expected_item.category)
  })
}

export function expect_powers_to_be_the_same (actual, expected) {
  expect(actual.id).to.be.equal(expected.id)
  expect(actual.name).to.be.equal(expected.name)
  expect(actual.description).to.be.equal(expected.description)
  expect(actual.rank).to.be.equal(expected.rank)
  expect(actual.power_points).to.be.equal(expected.power_points)
  expect(actual.range).to.be.equal(expected.range)
  expect(actual.duration).to.be.equal(expected.duration)
  expect(actual.trappings).to.be.equal(expected.trappings)
}

export function expect_power_lists_to_be_the_same (actual, expected) {
  expect(actual.length).to.be.equal(expected.length)
  actual.forEach((actual_item, index) => {
    expect_powers_to_be_the_same(actual_item, expected[index])
  })
}

export function expect_skill_to_be_the_same (actual, expected) {
  expect(actual.id).to.be.equal(expected.id)
  expect(actual.name).to.be.equal(expected.name)
  expect(actual.description).to.be.equal(expected.description)
  expect(actual.ability).to.be.equal(expected.ability)
}

export function expect_skill_lists_to_be_the_same (actual, expected) {
  if (expected) {
    expect(actual.length).to.be.equal(expected.length)
    actual.map((actual_item, index) => {
      let expected_item = expected[index]
      expect_skill_to_be_the_same(actual_item, expected_item)
    })
  } else {
    expect(actual).to.be.not.ok
  }
}

export function expect_techology_levels_to_be_the_same (actual, expected) {
  expect(actual.id).to.be.equal(expected.id)
  expect(actual.name).to.be.equal(expected.name)
  expect(actual.description).to.be.equal(expected.description)
}

export function expect_note_lists_to_be_the_same (actual, expected) {
  expect(actual.length).to.be.equal(expected.length)
  actual.map((actual_item, index) => {
    let expected_item = expected[index]
    expect(actual_item.id).to.be.equal(expected_item.id)
    expect(actual_item.name).to.be.equal(expected_item.name)
    expect(actual_item.description).to.be.equal(expected_item.description)
  })
}
