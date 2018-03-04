import {armor_technology_level} from '../data'

export default function (obj, args, context, graphql) {
  return armor_technology_level(obj.id)
}