import React from 'react'
// import {connect} from 'react-redux'
// import {withRouter} from 'react-router-dom'

export default function (Component) {

  class AuthenticatedComponent extends React.Component {

    componentWillMount () {
      this.checkAuth(this.props.isAuthenticated)
    }

    componentWillReceiveProps (nextProps) {
      this.checkAuth(nextProps.isAuthenticated)
    }

    checkAuth (isAuthenticated) {
      if (!isAuthenticated) {
        this.props.history.push('/login')
      }
    }

    render () {
      return (
        <div >
          {this.props.isAuthenticated === true
            ? <Component {...this.props}/>
            : null
          }
        </div >
      )

    }
  }

  // const mapStateToProps = (state) => ({
  //   token: state.auth.token,
  //   userName: state.auth.userName,
  //   isAuthenticated: state.auth.isAuthenticated
  // })
  //
  // const mapDispatchToProps = (dispatch) => {
  //   return {}
  // }

  // return withRouter(connect(mapStateToProps, mapDispatchToProps)(AuthenticatedComponent))
  return AuthenticatedComponent
}
