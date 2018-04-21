package org.savageworlds.vtt.virtualtabletopapi.repositories;

import org.savageworlds.vtt.virtualtabletopapi.models.GearCategory;
import org.springframework.data.repository.PagingAndSortingRepository;
import org.springframework.data.rest.core.annotation.RepositoryRestResource;

import java.util.List;

@RepositoryRestResource
public interface GearCategoryRepository extends PagingAndSortingRepository<GearCategory, Long> {

}
