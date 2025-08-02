import { EmailFormGroup, PasswordFormGroup } from 'bootstrap-react-components';
import React, { useState } from 'react';
import { useNavigate } from 'react-router-dom';
import { useAuth } from '../../contexts/AuthContext';
import './form_signin.css';

export default function Register() {
  const navigate = useNavigate();
  const { register, error: authError, loading } = useAuth();
  
  const [formData, setFormData] = useState({
    username: '',
    password: '',
    confirmPassword: ''
  });
  
  const [errors, setErrors] = useState({
    username: '',
    password: '',
    confirmPassword: ''
  });

  const handleChange = (field) => (e) => {
    const value = e.target.value;
    setFormData({
      ...formData,
      [field]: value
    });
    
    // Validate on change for better UX
    const newErrors = { ...errors };
    
    if (field === 'username') {
      const emailRegex = /^[a-zA-Z0-9.!#$%&'*+/=?^_`{|}~-]+@[a-zA-Z0-9](?:[a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?(?:\.[a-zA-Z0-9](?:[a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?)*$/;
      if (value && !emailRegex.test(value)) {
        newErrors.username = 'Please enter a valid email address';
      } else {
        newErrors.username = '';
      }
    }
    
    if (field === 'password') {
      if (value && value.length < 8) {
        newErrors.password = 'Password must be at least 8 characters long';
      } else {
        newErrors.password = '';
      }
      // Check if confirm password matches
      if (formData.confirmPassword && value !== formData.confirmPassword) {
        newErrors.confirmPassword = "Passwords don't match";
      } else if (formData.confirmPassword && value === formData.confirmPassword) {
        newErrors.confirmPassword = '';
      }
    }
    
    if (field === 'confirmPassword') {
      if (value && formData.password && value !== formData.password) {
        newErrors.confirmPassword = "Passwords don't match";
        newErrors.password = "Passwords don't match";
      } else if (value && formData.password && value === formData.password) {
        newErrors.confirmPassword = '';
        newErrors.password = '';
      }
    }
    
    setErrors(newErrors);
  };

  const validateForm = () => {
    const newErrors = {
      username: '',
      password: '',
      confirmPassword: ''
    };
    
    let isValid = true;
    const emailRegex = /^[a-zA-Z0-9.!#$%&'*+/=?^_`{|}~-]+@[a-zA-Z0-9](?:[a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?(?:\.[a-zA-Z0-9](?:[a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?)*$/;
    
    if (!emailRegex.test(formData.username)) {
      newErrors.username = 'Please enter a valid email address';
      isValid = false;
    }
    
    if (!formData.password || formData.password.length < 8) {
      newErrors.password = 'Password must be at least 8 characters long';
      isValid = false;
    }
    
    if (formData.password !== formData.confirmPassword) {
      newErrors.password = "Passwords don't match";
      newErrors.confirmPassword = "Passwords don't match";
      isValid = false;
    }
    
    setErrors(newErrors);
    return isValid;
  };

  const handleSubmit = async (e) => {
    e.preventDefault();
    
    // Force validation to show all errors
    const isValid = validateForm();
    
    if (!isValid) {
      // Give React time to render the validation errors
      setTimeout(() => {}, 100);
      return;
    }
    
    const result = await register({
      username: formData.username, // Use email as username for backend compatibility
      email: formData.username,
      password: formData.password
    });
    
    if (result.success) {
      navigate('/');
    }
  };

  return (
    <div id="RegisterPage" className="container mt-5">
      <div className="row justify-content-center">
        <div className="col-md-6">
          <form id="registrationForm" className="form-signin card p-4" onSubmit={handleSubmit}>
            <h2 className="form-signin-heading mb-4">Please Register</h2>
            
            {authError && (
              <div className="alert alert-danger" role="alert">
                {authError}
              </div>
            )}
            
            <div className="mb-3">
              <EmailFormGroup
                id="username"
                label="Email"
                value={formData.username}
                required={true}
                onChange={handleChange('username')}
                error={errors.username}
              />
              {errors.username && (
                <div className="invalid-feedback d-block text-danger">
                  {errors.username}
                </div>
              )}
            </div>
            
            <div className="mb-3">
              <PasswordFormGroup
                id="password"
                label="Password"
                value={formData.password}
                required={true}
                onChange={handleChange('password')}
                error={errors.password}
              />
              {errors.password && (
                <div className="invalid-feedback d-block text-danger">
                  {errors.password}
                </div>
              )}
            </div>
            
            <div className="mb-3">
              <PasswordFormGroup
                id="confirmPassword"
                label="Confirm Password"
                value={formData.confirmPassword}
                required={true}
                onChange={handleChange('confirmPassword')}
                error={errors.confirmPassword}
              />
              {errors.confirmPassword && (
                <div className="invalid-feedback d-block text-danger">
                  {errors.confirmPassword}
                </div>
              )}
            </div>
            
            <button
              id="registerUserButton"
              type="submit"
              className="btn btn-success btn-lg btn-block"
              disabled={loading}
            >
              {loading ? 'Registering...' : 'Register'}
            </button>
            
            <div className="mt-3 text-center">
              <a href="/login" className="btn btn-link">
                Already have an account? Login
              </a>
            </div>
          </form>
        </div>
      </div>
    </div>
  );
}