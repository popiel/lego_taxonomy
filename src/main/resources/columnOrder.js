const COLUMN_IDS = ['category', 'category2', 'category3', 'category4', 'image', 'color', 'quantity', 'name', 'partNumber'];
const CATEGORY_COLUMNS = ['category', 'category2', 'category3', 'category4'];
const CATEGORY_START = 0;
const CATEGORY_END = 3;

function getColumnIndex(order, columnId) {
  return order.indexOf(columnId);
}

function isCategoryColumn(columnId) {
  return CATEGORY_COLUMNS.includes(columnId);
}

function constrainDropTarget(targetIndex, isDraggingCategory) {
  if (isDraggingCategory) {
    return targetIndex;
  }
  if (targetIndex >= CATEGORY_START && targetIndex <= CATEGORY_END) {
    return CATEGORY_END + 1;
  }
  return targetIndex;
}

function moveColumn(order, fromIndex, toIndex) {
  const newOrder = [...order];
  const [moved] = newOrder.splice(fromIndex, 1);
  const insertIndex = toIndex > fromIndex && toIndex > CATEGORY_END + 1 ? toIndex - 1 : toIndex;
  newOrder.splice(insertIndex, 0, moved);
  return newOrder;
}

function moveCategoryGroup(order, fromIndex, toIndex) {
  const newOrder = [...order];
  const categoryColumnsInOrder = newOrder.filter(col => CATEGORY_COLUMNS.includes(col));
  if (categoryColumnsInOrder.length === 0) return newOrder;
  
  const firstCategoryIndex = newOrder.findIndex(col => CATEGORY_COLUMNS.includes(col));
  const isDraggingFromCategoryRange = fromIndex <= CATEGORY_END;
  
  if (!isDraggingFromCategoryRange) {
    return moveColumn(order, fromIndex, toIndex);
  }
  
  const targetIsInCategoryRange = toIndex <= CATEGORY_END;
  if (targetIsInCategoryRange) {
    return [...order];
  }
  
  const categoryCount = categoryColumnsInOrder.length;
  let adjustedToIndex = toIndex;
  
  if (toIndex > fromIndex) {
    adjustedToIndex = toIndex - categoryCount;
  } else {
    adjustedToIndex = Math.max(toIndex - categoryCount + 1, CATEGORY_END + 1);
  }
  
  const removedCategories = newOrder.splice(firstCategoryIndex, categoryCount);
  newOrder.splice(adjustedToIndex, 0, ...removedCategories);
  
  return newOrder;
}

function resetColumns() {
  return [...COLUMN_IDS];
}

module.exports = {
  COLUMN_IDS,
  CATEGORY_COLUMNS,
  CATEGORY_START,
  CATEGORY_END,
  getColumnIndex,
  isCategoryColumn,
  constrainDropTarget,
  moveColumn,
  moveCategoryGroup,
  resetColumns
};
