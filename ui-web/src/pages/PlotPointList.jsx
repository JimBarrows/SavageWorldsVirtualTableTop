import { FontAwesomeIcon } from '@fortawesome/react-fontawesome';
import { Button, PageHeader } from 'bootstrap-react-components';
import React from 'react';
import { useNavigate } from 'react-router-dom';
import { useQuery } from 'react-query';
import PlotPointList from '../components/plotpoint/list/index';
import { plotPointService } from '../services';

export default function PlotPointListPage() {
  const navigate = useNavigate();
  
  // Fetch plot points using React Query
  const { data, isLoading, isError, error } = useQuery(
    ['plotPoints'],
    () => plotPointService.getPlotPoints(1, 50) // Fetch first 50 plot points
  );

  const navigateToNewPlotPoint = () => navigate('/plot_point/add');

  const plotPointList = () => (
    <PlotPointList 
      id={'mainPlotPointList'} 
      plotPoints={data?.data || []} 
    />
  );

  if (isLoading) {
    return (
      <div id='PlotPointListPage'>
        <PageHeader id='PlotPointListPage'><h1>Plot Points</h1></PageHeader>
        <div className="text-center mt-5">
          <div className="spinner-border" role="status">
            <span className="sr-only">Loading...</span>
          </div>
          <p className="mt-2">Loading plot points...</p>
        </div>
      </div>
    );
  }

  if (isError) {
    return (
      <div id='PlotPointListPage'>
        <PageHeader id='PlotPointListPage'><h1>Plot Points</h1></PageHeader>
        <div className="alert alert-danger mt-3" role="alert">
          <h4 className="alert-heading">Error loading plot points</h4>
          <p>{error?.message || 'An unexpected error occurred'}</p>
          <hr />
          <p className="mb-0">
            Please try refreshing the page or contact support if the problem persists.
          </p>
        </div>
      </div>
    );
  }

  const plotPoints = data?.data || [];

  return (
    <div id='PlotPointListPage'>
      <PageHeader id='PlotPointListPage'><h1>Plot Points</h1></PageHeader>
      <Button id='addPlotPoint' onClick={navigateToNewPlotPoint}>
        <FontAwesomeIcon icon={'plus'} />&nbsp;Add
      </Button>
      {plotPoints.length > 0 ? plotPointList() : (
        <p className="mt-3">There are no plot points, please add one</p>
      )}
      {data?.pagination && (
        <div className="mt-3">
          <small className="text-muted">
            Showing {plotPoints.length} of {data.pagination.total} plot points
          </small>
        </div>
      )}
    </div>
  );
}