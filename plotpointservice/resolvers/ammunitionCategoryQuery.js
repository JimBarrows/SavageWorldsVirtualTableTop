import {ammunition_category} from '../data'

export default function (obj, args, context, graphql) {

  return ammunition_category(obj.id)
}