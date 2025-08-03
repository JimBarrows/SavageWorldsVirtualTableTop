import React from 'react';

const ResetPasswordPage = () => {
  return (
    <div className="container mt-5">
      <div className="row justify-content-center">
        <div className="col-md-6">
          <div className="card">
            <div className="card-header">
              <h3>Reset Password</h3>
            </div>
            <div className="card-body">
              <p>Password reset functionality will be implemented in a future update.</p>
              <a href="/login" className="btn btn-primary">
                Back to Login
              </a>
            </div>
          </div>
        </div>
      </div>
    </div>
  );
};

export default ResetPasswordPage;