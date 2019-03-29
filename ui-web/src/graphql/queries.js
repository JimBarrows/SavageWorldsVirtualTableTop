// eslint-disable
// this is an auto generated file. This will be overwritten

export const getPlotPoint   = `query GetPlotPoint($id: ID!) {
  getPlotPoint(id: $id) {
    id
    name
    description
    basic_rules {
      maximumTraitPoints
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
    gear {
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
      ... on HandWeapons {
        damage {
          attribute
        }
      }
      ... on Ammunition {
        armor
      }
      ... on RangedWeapon {
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
      ... on VehicleAndAtMounted {
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
      ... on SpecialWeapon {
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
      basic_rules {
        maximumTraitPoints
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
      gear {
        name
        description
        cost
        weight
        note
        ... on Ammunition {
          armor
        }
        ... on RangedWeapon {
          shortRange
          mediumRange
          longRange
          rateOfFire
          shots
          minimumStrength
        }
        ... on VehicleAndAtMounted {
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
        ... on SpecialWeapon {
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
    }
    nextToken
  }
}
`;
