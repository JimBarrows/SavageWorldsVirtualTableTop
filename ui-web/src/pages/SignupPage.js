import React, { Component } from 'react'
import { Link } from 'react-router-dom'
import { withRouter } from '../utils/withRouter'
import SignupForm from '../components/SignupForm'
import authService from '../services/authService'

class SignupPage extends Component {
  state = {
    successMessage: null,
    errorMessage: null
  }

  handleSubmit = async (formData) => {
    try {
      await authService.register(formData)
      
      this.setState({ 
        successMessage: 'Account created successfully! Redirecting to login...',
        errorMessage: null 
      })

      // Redirect to login page after 2 seconds
      setTimeout(() => {
        this.props.navigate('/login')
      }, 2000)
    } catch (error) {
      this.setState({ 
        errorMessage: error.message || 'An error occurred during signup',
        successMessage: null 
      })
      throw error // Re-throw to let the form handle it
    }
  }

  render() {
    const { successMessage, errorMessage } = this.state

    return (
      <div className="container mt-5">
        <div className="row justify-content-center">
          <div className="col-xs-12 col-sm-8 col-md-6 col-lg-4">
            <div className="card">
              <div className="card-body">
                <h2 className="text-center mb-4">Sign Up</h2>
                <p className="text-center text-muted mb-4">Create your account</p>
                
                {successMessage && (
                  <div className="alert alert-success mb-3">
                    {successMessage}
                  </div>
                )}
                
                {errorMessage && (
                  <div className="alert alert-danger mb-3" role="alert">
                    {errorMessage}
                  </div>
                )}
                
                <SignupForm onSubmit={this.handleSubmit} />
                
                <hr className="my-4" />
                
                <p className="text-center mb-0">
                  Already have an account? <Link to="/login">Log in</Link>
                </p>
              </div>
            </div>
          </div>
        </div>
      </div>
    )
  }
}

export default withRouter(SignupPage)