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
    setFormData({
      ...formData,
      [field]: e.target.value
    });
    
    // Clear error for this field when user starts typing
    setErrors({
      ...errors,
      [field]: ''
    });
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
      newErrors.username = 'Username must be a valid email address';
      isValid = false;
    }
    
    if (!formData.password || formData.password.length < 8) {
      newErrors.password = 'Password must be at least 8 characters';
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
    
    if (!validateForm()) {
      return;
    }
    
    const result = await register({
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
            
            <EmailFormGroup
              id="username"
              label="Email"
              value={formData.username}
              required={true}
              onChange={handleChange('username')}
              error={errors.username}
            />
            
            <PasswordFormGroup
              id="password"
              label="Password"
              value={formData.password}
              required={true}
              onChange={handleChange('password')}
              error={errors.password}
            />
            
            <PasswordFormGroup
              id="confirmPassword"
              label="Confirm Password"
              value={formData.confirmPassword}
              required={true}
              onChange={handleChange('confirmPassword')}
              error={errors.confirmPassword}
            />
            
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