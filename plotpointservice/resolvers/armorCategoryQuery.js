import {armor_category} from '../data'

export default function (obj, args, context, graphql) {
  return armor_category(obj.id)
}