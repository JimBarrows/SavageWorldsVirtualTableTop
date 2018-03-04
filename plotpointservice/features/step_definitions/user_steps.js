import {setContext} from 'apollo-link-context'
import {Given} from 'cucumber'
import gql from 'graphql-tag'
import 'isomorphic-fetch'

Given('a user registered with username = {string}, password = {string}', function (user_id, password) {
  return this.user_db.one('insert into user_login (user_id, password) values ($1, $2) returning id', [user_id, password])
  .then(data => this.user = {id: data.id, user_id: user_id, password: password})
})

Given('I have logged in', function () {
  this.result = {
    data: null,
    error: null
  }
  return this.userService.mutate({
                                   mutation: gql `mutation authenticate($user_id: String!, $password: String!) { authenticate(user_id: $user_id, password: $password) { token, user_id, id}}`,
                                   variables: {
                                     user_id: this.user.user_id,
                                     password: this.user.password
                                   }
                                 })
  .then(response => {
    this.user = response.data.authenticate
    const authLink = setContext((_, {headers}) => {
      return {
        headers: {
          ...headers,
          authorization: `Bearer ${response.data.authenticate.token}`
        }
      }
    })
    this.plotPointService.link = authLink.concat(this.plotPointService.link)
  })
})