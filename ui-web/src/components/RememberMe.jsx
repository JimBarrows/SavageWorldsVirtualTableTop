import React from 'react';
import PropTypes from 'prop-types';

const RememberMe = ({ checked, onChange }) => {
  const handleChange = (event) => {
    onChange(event.target.checked);
  };

  return (
    <div className="form-check">
      <input
        className="form-check-input"
        type="checkbox"
        id="rememberMe"
        checked={checked}
        onChange={handleChange}
        aria-label="Remember Me"
      />
      <label className="form-check-label" htmlFor="rememberMe">
        Remember Me
      </label>
    </div>
  );
};

RememberMe.propTypes = {
  checked: PropTypes.bool.isRequired,
  onChange: PropTypes.func.isRequired
};

export default RememberMe;