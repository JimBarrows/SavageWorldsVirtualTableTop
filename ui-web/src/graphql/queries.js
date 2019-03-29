// eslint-disable
// this is an auto generated file. This will be overwritten

export const getPlotPoint   = `query GetPlotPoint($id: ID!) {
  getPlotPoint(id: $id) {
    id
    name
    description
    basicRules {
      maximumAttributePoints
      maximumMajorHindrances
      maximumMinorHindrances
      maximumSkillPoints
    }
    beasts {
      agility {
        dice
        bonus
      }
      animalIntelligence
      armor
      charimsa
      description
      name
      pace
      skills {
        name
        description
        attribute
      }
      smarts {
        dice
        bonus
      }
      specialAbilities {
        name
        description
      }
      spirit {
        dice
        bonus
      }
      strength {
        dice
        bonus
      }
      vigor {
        dice
        bonus
      }
    }
    characters {
      agility {
        dice
        bonus
      }
      animalIntelligence
      armor
      charimsa
      description
      edges {
        description
      }
      gear {
        quantity
        notes
      }
      hindrances {
        selectedSeverity
        description
      }
      name
      pace
      skills {
        name
        description
        attribute
      }
      smarts {
        dice
        bonus
      }
      specialAbilities {
        name
        description
      }
      spirit {
        dice
        bonus
      }
      strength {
        dice
        bonus
      }
      vigor {
        dice
        bonus
      }
    }
    edges {
      name
      description
      category {
        name
        description
      }
      requirements {
        name
        description
      }
      effects
    }
    gear_eras {
      name
      description
    }
    gear_kinds {
      name
      description
    }
    ammunition {
      name
      description
      cost
      weight
      note
      era {
        name
        description
      }
      kind {
        name
        description
      }
      armor
    }
    armor {
      name
      description
      cost
      weight
      note
      era {
        name
        description
      }
      kind {
        name
        description
      }
    }
    mundaneItems {
      name
      description
      cost
      weight
      note
      era {
        name
        description
      }
      kind {
        name
        description
      }
    }
    handWeapons {
      name
      description
      cost
      weight
      note
      era {
        name
        description
      }
      kind {
        name
        description
      }
      damage {
        attribute
      }
    }
    rangedWeapons {
      name
      description
      cost
      weight
      note
      era {
        name
        description
      }
      kind {
        name
        description
      }
      shortRange
      mediumRange
      longRange
      damage {
        attribute
      }
      rateOfFire
      shots
      minimumStrength
    }
    specialWeapons {
      name
      description
      cost
      weight
      note
      era {
        name
        description
      }
      kind {
        name
        description
      }
      shortRange
      mediumRange
      longRange
      damage {
        attribute
      }
      rateOfFire
      shots
      minimumStrength
      burstTemplate
      military
      armorPiercing
    }
    vehicleAndAtMountedWeapons {
      name
      description
      cost
      weight
      note
      era {
        name
        description
      }
      kind {
        name
        description
      }
      shortRange
      mediumRange
      longRange
      damage {
        attribute
      }
      rateOfFire
      shots
      minimumStrength
      apDamage {
        attribute
      }
      apArmorPiercing
      heDamage {
        attribute
      }
      heBurstTemplate
      heArmorPiercing
    }
    hindrances {
      name
      description
      severity
    }
    powers {
      name
      description
      rank
      powerPoints
      range
      duration
      availableTo {
        name
        description
        effects
        skillName
      }
      trappings {
        name
        description
      }
    }
    arcaneBackgrounds {
      name
      description
      category {
        name
        description
      }
      requirements {
        name
        description
      }
      effects
      skillName
    }
    races {
      name
      description
      abilities {
        name
        description
        effects
        cost
      }
    }
    settingRules {
      name
      description
    }
    skills {
      name
      description
      attribute
    }
    airVehicles {
      acceleration
      armor
      crew
      description
      era {
        name
        description
      }
      kind {
        name
        description
      }
      maximumCost
      minimumCost
      name
      note
      passengers
      topSpeed
      toughness
      climb
    }
    waterVehicles {
      acceleration
      armor
      crew
      description
      era {
        name
        description
      }
      kind {
        name
        description
      }
      maximumCost
      minimumCost
      name
      note
      passengers
      topSpeed
      toughness
    }
    groundVehicles {
      acceleration
      armor
      crew
      description
      era {
        name
        description
      }
      kind {
        name
        description
      }
      maximumCost
      minimumCost
      name
      note
      passengers
      topSpeed
      toughness
    }
  }
}
`;
export const listPlotPoints = `query ListPlotPoints(
  $filter: ModelPlotPointFilterInput
  $limit: Int
  $nextToken: String
) {
  listPlotPoints(filter: $filter, limit: $limit, nextToken: $nextToken) {
    items {
      id
      name
      description
      basicRules {
        maximumAttributePoints
        maximumMajorHindrances
        maximumMinorHindrances
        maximumSkillPoints
      }
      beasts {
        animalIntelligence
        armor
        charimsa
        description
        name
        pace
      }
      characters {
        animalIntelligence
        armor
        charimsa
        description
        name
        pace
      }
      edges {
        name
        description
        effects
      }
      gear_eras {
        name
        description
      }
      gear_kinds {
        name
        description
      }
      ammunition {
        name
        description
        cost
        weight
        note
        armor
      }
      armor {
        name
        description
        cost
        weight
        note
      }
      mundaneItems {
        name
        description
        cost
        weight
        note
      }
      handWeapons {
        name
        description
        cost
        weight
        note
      }
      rangedWeapons {
        name
        description
        cost
        weight
        note
        shortRange
        mediumRange
        longRange
        rateOfFire
        shots
        minimumStrength
      }
      specialWeapons {
        name
        description
        cost
        weight
        note
        shortRange
        mediumRange
        longRange
        rateOfFire
        shots
        minimumStrength
        burstTemplate
        military
        armorPiercing
      }
      vehicleAndAtMountedWeapons {
        name
        description
        cost
        weight
        note
        shortRange
        mediumRange
        longRange
        rateOfFire
        shots
        minimumStrength
        apArmorPiercing
        heBurstTemplate
        heArmorPiercing
      }
      hindrances {
        name
        description
        severity
      }
      powers {
        name
        description
        rank
        powerPoints
        range
        duration
      }
      arcaneBackgrounds {
        name
        description
        effects
        skillName
      }
      races {
        name
        description
      }
      settingRules {
        name
        description
      }
      skills {
        name
        description
        attribute
      }
      airVehicles {
        acceleration
        armor
        crew
        description
        maximumCost
        minimumCost
        name
        note
        passengers
        topSpeed
        toughness
        climb
      }
      waterVehicles {
        acceleration
        armor
        crew
        description
        maximumCost
        minimumCost
        name
        note
        passengers
        topSpeed
        toughness
      }
      groundVehicles {
        acceleration
        armor
        crew
        description
        maximumCost
        minimumCost
        name
        note
        passengers
        topSpeed
        toughness
      }
    }
    nextToken
  }
}
`;
