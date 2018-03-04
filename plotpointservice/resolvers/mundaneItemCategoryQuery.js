import {mundane_item_category} from '../data'

export default function (obj, args, context, graphql) {
  return mundane_item_category(obj.id)
}