import {mundane_item_technology_level} from '../data'

export default function (obj, args, context, graphql) {
  return mundane_item_technology_level(obj.id)
}