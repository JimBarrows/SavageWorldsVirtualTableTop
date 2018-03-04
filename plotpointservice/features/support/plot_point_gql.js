import gql from 'graphql-tag'

const query = gql `
query plotPoints($pagination: Pagination!) {
  plotPoints(pagination: $pagination) {
    id
    name
    brief_description
    description
    ammunition(pagination: $pagination) {
      id
      name
      description
      weight
      cost
      category {
        id
        name
        description
      }
      notes {
        id
        name
        description
      }
    }
    arcaneBackgrounds(pagination: $pagination) {
      id
      name
      description
      starting_power_points
      starting_powers
      skill {
        id
        name
        description
        ability
      }
    }
    armors(pagination: $pagination) {
      id
      armor
      cost
      description
      name
      weight
      category {
        id
        name
        description
      }
      notes {
        id
        name
        description
      }
      technology_level {
        id
        name
        description
      }
    }
    beasts(pagination: $pagination) {
      id
      name
      description
      agility
      smarts
      spirit
      strength
      vigor
      animal_intelligence
      pace
      parry
      toughness
      armor
      wild_card
      skills {
        id
        name
        description
        ability
        value
        bonus
      }
      special_abilities {
        id
        name
        description
      }
    }
    characters(pagination: $pagination) {
      id
      agility
      armor
      charisma
      description
      experience_points
      name
      pace
      parry
      power_points
      rank
      smarts
      spirit
      strength
      toughness
      vigor
      wild_card
      ammunition {
        id
        name
        description
        weight
        cost
        category {
          id
          name
          description
        }
        notes {
          id
          name
          description
        }
        amount
      }
      edges {
        id
        name
        description
        effects
        requirements {
          id
          requirement
        }
        type {
          id
          name
          description
        }
      }
      hand_weapons {
        id
        cost
        damage
        description
        name
        category {
          id
          name
          description
        }
        notes {
          id
          name
          description
        }
        technology_level {
          id
          name
          description
        }
        weight
      }
      hindrances {
        id
        name
        description
        type
        taken_as
      }
      mundane_items {
        id
        cost
        description
        name
        category {
          id
          name
          description
        }
        notes {
          id
          name
          description
        }
        technology_level {
          id
          name
          description
        }
        weight
      }
      powers {
        id
        name
        power {
          id
          name
          description
          rank
          power_points
          range
          duration
          trappings
        }
        trapping {
          id
          name
          description
          effects {
            name
            description
          }
        }
        notes
      }
    }
    edges(pagination: $pagination) {
      id
      name
      description
      effects
      requirements {
        id
        requirement
      }
      type {
        id
        name
        description
      }
    }
    hand_weapons(pagination: $pagination) {
      id
      cost
      damage
      description
      name
      category {
        id
        name
        description
      }
      notes {
        id
        name
        description
      }
      technology_level {
        id
        name
        description
      }
      weight
    }
    hindrances(pagination: $pagination) {
      id
      name
      description
      type
    }
    mundane_items(pagination: $pagination) {
      id
      cost
      description
      name
      category {
        id
        name
        description
      }
      notes {
        id
        name
        description
      }
      technology_level {
        id
        name
        description
      }
      weight
    }
    powers(pagination: $pagination) {
      id
      name
      power_points
      description
      rank
      range
      duration
      trappings
      available_to {
        id
        name
        description
        skill {
          id
          name
          description
          ability
        }
        starting_power_points
        starting_powers
      }
    }
    skills(pagination: $pagination) {
      id
      name
      description
      ability
    }

  }
}

`

export default query
