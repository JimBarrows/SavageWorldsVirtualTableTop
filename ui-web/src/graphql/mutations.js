/* eslint-disable */
// this is an auto generated file. This will be overwritten

export const createPlotPoint = /* GraphQL */ `
  mutation CreatePlotPoint(
    $input: CreatePlotPointInput!
    $condition: ModelPlotPointConditionInput
  ) {
    createPlotPoint(input: $input, condition: $condition) {
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
      description
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
      gearEras {
        name
        description
      }
      gearKinds {
        name
        description
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
      hindrances {
        name
        description
        severity
      }
      id
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
      name
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
      settingRules {
        name
        description
      }
      skills {
        name
        description
        attribute
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
      createdAt
      updatedAt
      owner
    }
  }
`;
export const updatePlotPoint = /* GraphQL */ `
  mutation UpdatePlotPoint(
    $input: UpdatePlotPointInput!
    $condition: ModelPlotPointConditionInput
  ) {
    updatePlotPoint(input: $input, condition: $condition) {
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
      description
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
      gearEras {
        name
        description
      }
      gearKinds {
        name
        description
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
      hindrances {
        name
        description
        severity
      }
      id
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
      name
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
      settingRules {
        name
        description
      }
      skills {
        name
        description
        attribute
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
      createdAt
      updatedAt
      owner
    }
  }
`;
export const deletePlotPoint = /* GraphQL */ `
  mutation DeletePlotPoint(
    $input: DeletePlotPointInput!
    $condition: ModelPlotPointConditionInput
  ) {
    deletePlotPoint(input: $input, condition: $condition) {
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
      description
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
      gearEras {
        name
        description
      }
      gearKinds {
        name
        description
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
      hindrances {
        name
        description
        severity
      }
      id
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
      name
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
      settingRules {
        name
        description
      }
      skills {
        name
        description
        attribute
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
      createdAt
      updatedAt
      owner
    }
  }
`;
